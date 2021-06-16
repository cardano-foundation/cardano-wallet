#!/usr/bin/env ruby

######################################################################
# Script for analysing recent CI failures.
#
# Usage:
#   ./scripts/bors-stats.rb --help
#
# Configuration:
#
# You need to set the GITHUB_API_TOKEN and BUILDKITE_API_TOKEN
# environment variables before running this script, so that the
# respective APIs can be queried.
#
# Alternatively, you can configure BORS_STATS_PASSWORD_COMMAND to
# fetch those secrets from your password store. For example:
#   export BORS_STATS_PASSWORD_COMMAND="pass show"
#
# Requirements:
#  - Ruby 2.6 (or greater)
#  - The 'thor' Ruby Gem
#  - curl (check that brotli support is enabled with curl -V)
#
# To automatically get all dependencies, just run this from nix-shell.
#
######################################################################

require 'date'
require 'digest'
require 'fileutils'
require 'json'
require 'open3'
require 'thor'
require 'tmpdir'

######################################################################
## Configuration variables and CLI

$envGithubApiToken    = "GITHUB_API_TOKEN"
$envBuildkiteApiToken = "BUILDKITE_API_TOKEN"
$envPasswordCommand   = "BORS_STATS_PASSWORD_COMMAND"
$envDebug             = "BORS_STATS_DEBUG"

##
# The CLI option parser for `bors-stats.rb`.

class BorsStats < Thor
  map "-a" => :a
  class_option :count, :type => :numeric, :required => false, :default => 100, :desc => "How many PRs to fetch"
  class_option "auto-annotate", :type => :boolean, :required => false, :default => false, :desc => "Look for hspec failures and add as tags"
  class_option "auto-annotate-fuzzy", :type => :boolean, :required => false, :default => false, :desc => "Look for hspec failures and add as tags (fuzzily)"
  class_option "fetch-system", :type => :boolean, :required => false, :default => false, :desc => "Scrape and add mac or linux as tag"
  class_option "force-refetch", :type => :boolean, :required => false, :default => false, :desc => "Fetch new data even if recent cache exists."
  class_option :details, :type => :string, :required => false, :default => :auto, :desc => "Show details (true/false/auto)"
  class_option :search, :type => :string, :required => false, :default => nil, :desc => "Filter on the bodyText of failures"
  class_option :tag, :type => :string, :required => false, :desc => "Match this tag (e.g. #2040)"
  class_option :annotate, :type => :array, :required => false, :desc => "Convert matches in comments into tags"
  class_option :after, :type => :string, :required => false, :desc => "Only show failures after this date"
  class_option :before, :type => :string, :required => false, :desc => "Only show failures before this date"

  desc "list", "list all failures"
  def list()
    comments = fetch_comments_with_options options

    comments[:filtered].each do |c|
      puts c.pretty(showDetails = options[:details])
    end

    tm = fetch_gh_ticket_titlemap
    nTot = comments[:filtered].count
    nExcluded = comments[:unfiltered].length - nTot
    nSucc = comments[:filtered].filter { |x| x.succeeded }.length
    nFailed = nTot - nSucc

    puts ""
    puts "succeeded: " + (bold nSucc.to_s) + " failed: " + (bold nFailed.to_s) + " (" + (bold (failure_rate(nFailed, nTot))) + ") total: " + (bold nTot.to_s)
    puts "Excluding " + nExcluded.to_s + " #expected or #duplicate failures"
    puts ""
    puts "Broken down by tags/issues:"
    breakdown(comments[:filtered],tm).each do |k,v|
      t = k[:tag]
      n = k[:n]
      title = tm.dig t, "title"
      title = title.nil? ? "" : title
      puts (bold(n.to_s) + " times (" + bold(failure_rate(n, nTot)) + ") " + yellow(t) + " " + bold(title))
    end
  end

  desc "download-logs", "download all logs to the given dir"
  method_option :dir, :type => :string, :required => false, :desc => "Directory for saving logs (default: tag being filtered on)"
  def download_logs
    fetch_logs(options)
  end

  default_task :list

  desc "cause", "List failures and merged PRs to determine causes"
  option :details, :type => :string, :required => false, :default => :false, :deonesc => "Show details (true/false/auto)"
  option :count, :type => :numeric, :required => false, :default => 100, :desc => "How many PRs to fetch"
  def cause
    comments = fetch_comments_with_options(options)[:filtered]
    prs = fetch_merged_prs options[:count]
    to_events(comments, prs).each do |x|
      puts "\n"
      puts x.pretty(showDetails = options[:details])
    end
  end
end

def sendGithubGraphQLQuery(qry, force_refetch = false)
  return JSON.parse curl(
      url: "https://api.github.com/graphql",
      headers: ["Content-type: application/json"],
      token: $envGithubApiToken,
      data: {query: qry}.to_json,
      outfile: get_cache_file(qry),
      cacheTTL: force_refetch ? 0 : 3600
  )
end

##
# Fetch system from hydra api.
#
# Not sure how to get the the build that actually failed, not just the
# aggregate build, to use the json response, so a dirty workaround is
# to scrape the HTML.
#
def fetch_system_from_build_link(link, force_refetch: false)
    if link.start_with? "https://hydra.iohk.io" then
      res = hydra_fetch(url: link, force_refetch: force_refetch)
      return (res.include? "mac-mini") ? "mac" : "linux"
    end
end

def try_fetch_system(comment, force_refetch: false)
  comment.links.each do |l|
    os = fetch_system_from_build_link(l, force_refetch: force_refetch)
    if os then comment.tags += [os] end
  end
end

##
# A parsed bors comment corresponding to a succeeding or failing
# build.
BorsComment = Struct.new(:url, :bodyText, :links, :createdAt, :tags, :succeeded) do
  def causes
    self.tags.filter {|x| x.start_with? "#" }
  end

  def pretty(showDetails = :auto)
    if showDetails == "auto" then showDetails = :auto end
    if showDetails == "true" then showDetails = true end
    if showDetails == "false" then showDetails = false end
    details = links.join("\n") + "\n" + bodyText
    maybeDetails = ((((not succeeded) and causes.length == 0) and showDetails == :auto) or (showDetails == true) ) ? details : ""
    header = pretty_time + " " + pretty_tags + " " + (blue url)
    return (header + "\n" + maybeDetails)
  end
  def pretty_time
    t_str = createdAt.strftime("%d %b %H:%M")
    return succeeded ? (green t_str) : (red t_str)
  end

  def pretty_tags
    yellow (tags.join(", "))
  end
end

def fetch_comments(target = 100, before = nil, force_refetch = false)
  numberPRsToFetch = [100, target.to_i].min
  numberCommentsToFetch = 100
  beforeQ = if before then ", before: \"" + before + "\"" else "" end
  query = <<~END
    query { repository(name: "cardano-wallet", owner: "input-output-hk") {
      pullRequests(last: #{numberPRsToFetch} #{beforeQ}) { edges { cursor, node {
        comments(first: #{numberCommentsToFetch}) { edges { node {
          bodyText,
          body,
          createdAt,
          url,
          author {
              login
          }
        }}}
      }}}
    }}
  END
  response = sendGithubGraphQLQuery(query, force_refetch)
  firstEdge = response['data']['repository']['pullRequests']['edges']&.first
  cursor = if firstEdge then firstEdge['cursor'] else nil end
  res = response['data']['repository']['pullRequests']['edges']
      .map { |x| x['node']['comments']['edges']}
      .flatten
      .map { |x| x['node']}
      .filter { |x| x['author']['login'] == "iohk-bors" }
      .map do |x|
        tags = x['bodyText'].scan(/^#[\d\w\-]+/).to_a
        succ = x['bodyText'].include? "Build succeeded"

        # Matches on the link from e.g.
        # [ci/hydra-build:required](https://hydra.iohk.io/build/6623546)
        buildLinks = x['body'].scan(/^\s+\* \[[^\]]+\]\((https:\/\/[^)]+)/).map { |x| x[0] }

        # No point in adding hydra and buildkite tags if the build succeeded
        if not succ then
          buildLinks.each do |l|
            if l.start_with? "https://hydra.iohk.io" then
              tags += ["hydra"]
            elsif l.start_with? "https://buildkite" then
              tags += ["buildkite"]
            end
          end
        end


        createdAt = DateTime.parse(x['createdAt'])
        BorsComment.new(x['url'], x['bodyText'], buildLinks, createdAt, tags, succ)
      end
  remaining = target - numberPRsToFetch
  if !res.empty? and remaining > 0 then
      fetch_comments(remaining, cursor, force_refetch) + res
  else
      res
  end

end

MergedPR = Struct.new(:url, :number, :mergedAt, :title) do
  def to_s
    self.pretty
  end

  def pretty(showDetails = :auto)
    return magenta (self.pretty_time + " #" + number.to_s + " " + title + "\n" + url).to_s
  end

  def pretty_time
    return mergedAt.strftime("%d %b %H:%M")
  end
end

def fetch_merged_prs(target, before = nil)
  numberPRsToFetch = [100, target.to_i].min
  query = <<~END
    query { repository(name: "cardano-wallet", owner: "input-output-hk") {
      pullRequests(last: #{numberPRsToFetch}, states: MERGED) { edges { cursor, node {
        number,
        mergedAt,
        title,
        url
      }}}
    }}
  END
  response = sendGithubGraphQLQuery(query)
  firstEdge = response['data']['repository']['pullRequests']['edges']&.first
  cursor = if firstEdge then firstEdge['cursor'] else nil end
  res = response['data']['repository']['pullRequests']['edges']
      .map { |x| x['node'] }
      .map do |x|
        mergedAt = DateTime.parse(x['mergedAt'])
        MergedPR.new(x['url'], x['number'], mergedAt, x['title'])
      end

  remaining = target - numberPRsToFetch
  if !res.empty? and remaining > 0 then
      fetch_merged_prs(remaining, cursor) + res
  else
      res
  end
end

##
# Fetch github comments with the "Test failure" label, and create a map from
# issue number to title and url
#
# === Example return value
#
#   {2083=>[{"number"=>2083, "url"=>"https://github.com/input-output-hk/cardano-wallet/issues/2083", "title"=>"Windows integration" }
def fetch_gh_ticket_titlemap
  query = <<~END
    query { repository(name: "cardano-wallet", owner: "input-output-hk") {
      issues(labels: ["Test failure"], last: 100) { edges { node {
        number,
        url,
        title
      }}}
    }}
  END
  return sendGithubGraphQLQuery(query)['data']['repository']['issues']['edges']
    .map { |x| x['node'] }
    .group_by { |x| "#" + x['number'].to_s }
    .transform_values { |x| x[0] }
end

def breakdown(comments, tm)
  m = {}
  comments.each do |c|
    c.tags.each do |tag|
      m[tag] = m.fetch(tag, []) + [c]
    end
  end
  return m.collect {|tag,failures| {:tag => tag, :n => failures.length}}.sort_by {|x| x[:n] }.reverse
end

def to_events(comments, prs)
  x = comments + prs
  x = x.sort_by { |x| event_date x }
  return x
end

# Get a date, regardless if the input is a BorsComment or a MergedPR
def event_date(x)
    if x.is_a? BorsComment then
      x[:createdAt]
    elsif x.is_a? MergedPR then
      x[:mergedAt]
    else
      fail "eventDate: unknown input"
    end
end

def include_worthy_comment(c)
  not [
    (c.tags.include? '#expected'),
    (c.tags.include? '#duplicate'),
    (c.bodyText.start_with? 'try'),
    (c.bodyText.include? 'Canceled'),
    (c.bodyText.include? 'Merge conflict'),
    (c.bodyText.include? 'Rejected by too few approved reviews'),
    (c.bodyText.include? 'Already running a review')
  ].any?
end

def failure_rate(n, nTot)
  '%.0f%%' % (100.0 * n / nTot)
end

##
# Fetch and filter comments based on the CLI options (defined above)
def fetch_comments_with_options(options)
  comments = fetch_comments(target = options[:count], nil, options["force-refetch"]).sort_by { |x| event_date x }

  if options[:search] then
    comments = comments.filter {|x| x.bodyText.include? options[:search] }
  end

  if options[:annotate] then
    comments.each do |c|
      options[:annotate].each do |pattern|
        if c.bodyText.include? pattern then
          c.tags += [pattern]
        end
      end
    end
  end


  if options["fetch-system"] then
    comments.each do |c|
      try_fetch_system(c, force_refetch: options["force-refetch"]) unless c.succeeded
    end
  end

  if options[:tag] then
    comments = comments.filter {|x| x.tags.include? options[:tag] }
  end

  if options[:before] then
    d = Date.parse(options[:before])
    comments = comments.filter { |c| c.createdAt < d }
  end

  if options[:after] then
    d = Date.parse(options[:after])
    comments = comments.filter { |c| c.createdAt > d }
  end

  if options["auto-annotate"] then
    comments.each do |c|
      # 1) API Specifications, SHELLEY_TRANSACTIONS, TRANS_CREATE_01x - Single Output Transaction
      # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      matches = c.bodyText.scan(/\d+\) ([\w\s\d\-\_\,]+)/).to_a.map { |x| x[0] }
      c.tags += matches
    end
  end

  if options["auto-annotate-fuzzy"] then
    comments.each do |c|
      # 1) API Specifications, SHELLEY_TRANSACTIONS, TRANS_CREATE_01x - Single Output Transaction
      # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      #                                              ^^^^^^^^^^^^^^^^
      matches = c.bodyText.scan(/\d+\) ((\w)+[ \,]{0,2})+/).to_a.map { |x| x.first }
      c.tags += matches
    end
  end

  filteredComments = comments.filter { |x| include_worthy_comment x }
  return { :unfiltered => comments, :filtered => filteredComments }
end

######################################################################

def fetch_logs(options)
  def setup_logs_dir(options)
    # Dir path for writing logs to
    tag = options[:tag].to_s
    path = options[:dir] ? options[:dir] : tag.delete_prefix("#")
    if path.empty? then
      STDERR.puts "Output location --dir=DIR option not supplied."
      if tag.empty?
        STDERR.puts "Provide either a --dir=DIR or --tag=TAG argument"
      else
        STDERR.puts "Could not infer directory from --tag=#{tag} filter"
      end
      exit 1
    end
    path = "./#{path}"
    FileUtils.mkdir_p path
    return path
  end

  def log_file_path(ci, path, id)
    return "#{path}/#{ci}-#{id}.log"
  end

  def download_notice(build_url, dest)
    puts "Downloading logs for #{build_url} => #{dest}"
  end

  force_refetch = options["force-refetch"]

  path = setup_logs_dir(options)

  comments = fetch_comments_with_options options
  comments[:filtered].each do |c|
    c.links.each do |l|
      if l.start_with? "https://buildkite" then

        build_id = l.scan(/builds\/(\d+)/).map { |x| x[0] }.first.to_s

        build_url = "https://api.buildkite.com/v2/organizations/input-output-hk/pipelines/cardano-wallet/builds/#{build_id}"
        json = buildkite_fetch_api(url: build_url, outfile: get_cache_file(build_url), force_refetch: force_refetch)
        log_url = json["jobs"]
            .select { |j| j["name"] == "Stack Rebuild" }
            .map { |j| j["raw_log_url"]}
            .first
        outfile = log_file_path("buildkite", path, build_id)
        download_notice(l, outfile)
        buildkite_fetch(url: log_url, outfile: outfile, force_refetch: force_refetch)
      elsif l.start_with? "https://hydra" then
        # Fetch the html build info page
        html = hydra_fetch(url: l, force_refetch: force_refetch)
        # Search for something like:
        # href="https://hydra.iohk.io/build/6163056/nixlog/1/raw"
        # To find the link to the raw logs of the child build that actually failed.
        matches = html.scan(/(https:\/\/hydra\.iohk\.io\/build\/(\d+)\/nixlog\/\d\/raw)/)
        if not (matches == []) then
          child_log_url = matches.first[0]
          child_id = matches.first[1]

          # Download logs
          outfile = log_file_path("hydra", path, child_id)
          download_notice(l, outfile)
          logs = hydra_fetch(url: child_log_url, outfile: outfile, force_refetch: force_refetch)
          debug_trace "Child log URL for #{l} is #{child_log_url}"
        else
          puts "Couldn't find failing build log in " + l
        end
      end
    end
  end
end

######################################################################
# Utils

##
# Downloads a URL using the `curl` command.
#
# To enable response caching, the +outfile+ parameter must be set and
# +cacheTTL+ must be non-zero. A negative +cacheTTL+ means cache
# indefinitely.
#
# Params:
# +url:+:: the full URL to fetch
# +outfile:+:: optional filepath to store the result.
# +headers:+:: a list of request headers.
# +token:+:: optional bearer token for request auth.
# +data:+:: optional JSON request body for POST.
# +cacheTTL:+:: optional time in seconds to keep cached responses.
# +curlopts:+:: extra command line parameters for curl.
def curl(url:, outfile: nil, headers: [], token: nil, data: nil, cacheTTL: 0, curlopts: [])
  if not outfile.nil? and cacheTTL != 0 and File.file? outfile then
    modifiedAt = File.mtime outfile
    if cacheTTL < 0 or modifiedAt.utc > (Time.now - cacheTTL).utc then
      debug_trace "Using cached file #{outfile}"
      return File.read outfile
    end
    puts "Cached value old... refetching..."
  end

  cmd = ["curl", "--fail", "--show-error", "--silent",
         "--compressed", "--location", url]
  unless outfile.nil?
    cmd += ["--output", outfile]
  end
  unless data.nil?
    cmd += ["--data-binary", data]
  end
  unless token.nil?
    cmd += ["-H", make_auth_header(token)]
  end
  headers.each do |h|
    cmd += ["-H", h]
  end
  cmd += curlopts

  debug_trace cmd.join(" ")
  stdout_str, status = Open3.capture2(*cmd)
  if status.success? then
    if outfile.nil?
      return stdout_str
    else
      return File.read outfile
    end
  else
    FileUtils.rm_f(outfile) unless outfile.nil?
    # TODO: replace abort with fail, and selectively add exception recovery
    abort("Failed to run \"#{cmd.join(" ")}\" (exited with status #{status.exitstatus})")
  end
end

##
# Return the cache filepath corresponding to the given value.
def get_cache_file(val)
  # setup cache
  cacheDir = ENV.fetch("XDG_CACHE_HOME") do
    homedir = getenv("HOME")
    next homedir.empty? ? Dir.tmpdir() : "#{homedir}/.cache"
  end
  statsDir = "#{cacheDir}/bors-stats"
  FileUtils.mkdir_p statsDir
  # Make cache key from value
  key = Digest::SHA1.hexdigest val
  return "#{statsDir}/#{key}.json"
end

##
# Look up a token from the environment or the password command and
# produce a HTTP Authorization header containing that token.
#
# This will fail hard if the secret can't be found.
def make_auth_header(varName)
  def get_secret(name)
    secret = getenv(name)
    if secret.empty?
      passwordCommand = getenv($envPasswordCommand)
      if passwordCommand.empty?
        abort("#{name} environment variable must be set")
      else
        cmd = "#{passwordCommand} #{name}"
        debug_trace cmd
        secret, status = Open3.capture2(cmd)
        secret = secret.strip
        if secret.empty? or not status.success?
          abort("Could not get secret from command (#{status}): #{cmd}")
        end
      end
    end
    return secret
  end

  return "Authorization: Bearer #{get_secret(varName)}"
end

##
# Download a URL from Hydra.
def hydra_fetch(url:, outfile: nil, force_refetch: false)
  return curl(
    url: url,
    outfile: outfile.nil? ? get_cache_file(url) : outfile,
    cacheTTL: force_refetch ? 0 : -1
  )
end

##
# Fetch a Buildkite log file or API resource and return its content.
#
# Params:
# +url:+:: the full URL to fetch.
# +outfile:+:: optional filepath for file downloads.
# +headers:+:: extra request headers.
def buildkite_fetch(url:, outfile: nil, headers: [], force_refetch: false)
  return curl(
    url: url,
    outfile: outfile,
    headers: headers,
    token: $envBuildkiteApiToken,
    cacheTTL: force_refetch ? 0 : -1,
  )
end

##
# Fetch a Buildkite API resource and return its JSON-parsed content.
def buildkite_fetch_api(url:, outfile: nil, force_refetch: false)
  res = buildkite_fetch(
    url: url,
    outfile: outfile,
    headers: ["Accept: application/json"],
    force_refetch: force_refetch
  )
  return JSON.parse res
end

def getenv(name)
  return ENV.fetch(name, "").strip
end

def debug_trace(msg)
  if not getenv($envDebug).empty?
    STDERR.puts "trace: #{msg}"
  end
end

######################################################################
# ANSI color code helpers

def ansi_clear; "\e[0m" end

def bold(s)    "\e[1m" + s + ansi_clear end
def red(s)     "\e[31m" + s + ansi_clear end
def green(s)   "\e[32m" + s + ansi_clear end
def yellow(s)  "\e[33m" + s + ansi_clear end
def blue(s)    "\e[34m" + s + ansi_clear end
def magenta(s) "\e[35m" + s + ansi_clear end

######################################################################
# Main

BorsStats.start(ARGV)
