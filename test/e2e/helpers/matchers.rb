require "rspec/expectations"

RSpec::Matchers.define :have_http do |code|
    match do |response|
      response.code == code
    end
    failure_message do |response|
      %Q{
          The response did not return expected HTTP code!
          Expected code = #{code}
          Actual code = #{response.code}
          Actual response:

          #{response}
        }
    end
  end
