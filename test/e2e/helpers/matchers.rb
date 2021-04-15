require "rspec/expectations"

RSpec::Matchers.define :be_correct_and_respond do |code|
  match do |response|
    if code == 204
      response.code == code
    else
      ((response.code == code) && ({ 'content-type' => ['application/json;charset=utf-8'] } <= response.headers))
    end
  end
  failure_message do |response|
    method = response.request.http_method.to_s.split('::').last.upcase
    uri = response.request.last_uri
    body = response.request.options[:body]
    headers = response.request.options[:headers]

    "
        The response did not return expected HTTP code or header 'content-type: application/json'!
        Expected code = #{code}
        Actual code = #{response.code}

        Actual request:
        #{method} #{uri}
        #{'Body: ' + body if body}
        #{'Headers: ' + headers.to_s if headers}

        Actual response:
        #{response}

        Actual response headers:
        #{response.headers}

        Time: #{Time.now}
      "
  end
end

RSpec::Matchers.define :respond_with do |code|
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

        Actual response headers:
        #{response.headers}

        Time: #{Time.now}
      "
  end
end
