#!/usr/bin/env ruby

require 'cardano_wallet'

timeout = 100
threshold = Time.now + timeout

$stdout.sync = true

def is_connected?
  begin
    CardanoWallet.new.misc.network.information
    true
  rescue
    false
  end
end

while (is_connected? == false && (Time.now <= threshold))
  puts "Wallet is not up yet... will check again in 5 seconds"
  sleep 5
end

if is_connected?
  puts "Wallet is up and running!!!"
  exit 0
else
  puts "Didn't manage to connect to wallet within #{timeout}s!!!"
  exit 1
end
