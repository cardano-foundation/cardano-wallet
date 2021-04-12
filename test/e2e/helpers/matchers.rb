require "rspec/expectations"

RSpec::Matchers.define :have_http do |code|
  match do |response|
    response.code == code
  end
  failure_message do |response|
    method = response.request.http_method.to_s.split('::').last.upcase
    uri = response.request.last_uri
    body = response.request.options[:body]
    headers = response.request.options[:headers]

    "
        The response did not return expected HTTP code!
        Expected code = #{code}
        Actual code = #{response.code}

        Actual request:
        #{method} #{uri}
        #{'Body: ' + body if body}
        #{'Headers: ' + headers.to_s if headers}

        Actual response:
        #{response}

        Time: #{Time.now}
      "
  end
end

RSpec::Matchers.define :have_headers do |expected_headers|
  match do |response|
    expected_headers <= response.headers
  end
  failure_message do |response|
    method = response.request.http_method.to_s.split('::').last.upcase
    uri = response.request.last_uri
    body = response.request.options[:body]
    headers = response.request.options[:headers]

    "
        The response did not return expected HTTP header!
        Expected response headers = #{expected_headers}
        Actual response headers = #{response.headers}

        Actual request:
        #{method} #{uri}
        #{'Body: ' + body if body}
        #{'Headers: ' + headers.to_s if headers}

        Actual response:
        #{response}

        Time: #{Time.now}
      "
  end
end
