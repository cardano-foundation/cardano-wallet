# frozen_string_literal: true

require 'buildkit'
require 'httparty'

##
# Buildkite helper
class Buildkite
  include HTTParty

  attr_reader :org, :pipeline, :client

  def initialize
    @org = 'input-output-hk'
    @pipeline = 'cardano-wallet'
    @api_token = ENV.fetch('BUILDKITE_API_TOKEN', nil)
    @client = Buildkit.new(token: @api_token)
  end

  def build_numbers(options = { branch: 'master' })
    @client.pipeline_builds(@org, @pipeline, options).map { |b| b[:number] }
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

  # get job details for given build number: job_name => job_id
  def jobs(build_no)
    build_details(build_no)[:jobs].to_h { |j| [j[:name], j[:id]] }
  end

  # get artifacts for given job_id
  def artifacts(build_no, job_id)
    @client.job_artifacts(@org, @pipeline, build_no, job_id)
  end

  # get artifact url
  def artifact_url(build_no, job_id, filename)
    url = artifacts(build_no, job_id).select { |a| a[:filename] == filename }.map { |a| a[:download_url] }[0]
    r = self.class.get(url, follow_redirects: false,
                            headers: { 'Authorization' => "Bearer #{@api_token}" })
    r.to_hash['url']
  end
end
