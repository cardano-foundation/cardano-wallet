# frozen_string_literal: true

require 'buildkit'
require 'httparty'

##
# Buildkite helper
class Buildkite
  include HTTParty
  include Helpers::Utils

  attr_reader :org, :pipeline, :client

  def initialize(pipeline = 'cardano-wallet')
    @org = 'cardanofoundation'
    @pipeline = pipeline
    @api_token = ENV.fetch('BUILDKITE_API_TOKEN', nil)
    @client = Buildkit.new(token: @api_token)
  end

  def build_numbers(options = { branch: 'master' })
    num = @client.pipeline_builds(@org, @pipeline, options).map { |b| b[:number] }
    raise "No builds found for #{options}" if num.empty?

    num
  end

  def last_build_number(options = { branch: 'master' })
    build_numbers(options).first
  end

  def build_details(build_no)
    @client.build(@org, @pipeline, build_no)
  end

  def last_build_details(options = { branch: 'master' })
    build_details(last_build_number(options))
  end

  # get job details for given build number: job_key => job_id
  def jobs(build_no)
    build_details(build_no)[:jobs].to_h { |j| [j[:step_key], j[:id]] }
  end

  # get list of artifacts for given job_id
  def artifacts(build_no, job_id)
    @client.job_artifacts(@org, @pipeline, build_no, job_id)
  end

  # get artifact urls for given job_id
  def artifact_urls(build_no, job_id)
    artifacts(build_no, job_id).map { |a| a[:download_url] }
  end

  # artifact url is redirecting to s3 and requires auth,
  # so we need to follow it to retrieve the downloadable url
  def make_artifact_url_downloadable(url)
    r = self.class.get(url, follow_redirects: false,
                            headers: { 'Authorization' => "Bearer #{@api_token}" })
    r.to_hash['url']
  end

  # we assume that there is only one artifact for given job
  def get_artifact_url(build_no, job_id)
    make_artifact_url_downloadable(artifact_urls(build_no, job_id).first)
  end

  def get_artifacts(build_no, job_id, target_dir, target_file = 'binary-dist', unzip: true)
    url = get_artifact_url(build_no, job_id)
    wget(url, target_file)
    unzip_artifact(target_file, target_dir) if unzip
  end
end
