require "rspec/expectations"

RSpec::Matchers.define :have_http do |code|
    match do |response|
      response.code == code
    end
    failure_message do |response|
      %Q{
          The response did not return expected HTTP code!
          Expected = #{code}
          Actual = #{response.code}
          Response:
          
          #{response}
        }
    end
  end
