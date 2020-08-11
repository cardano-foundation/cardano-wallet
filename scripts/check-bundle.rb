#!/usr/bin/env ruby

############################################################################
# Checks that every executable required in the release package is
# present and works.
############################################################################

require 'open3'

tests = {
  "cardano-wallet-jormungandr" => [ "jormungandr", "jcli" ],
  "cardano-wallet" => [ "cardano-node", "cardano-cli", "bech32", "cardano-tx", "cardano-address" ]
}

if ARGV.length < 1 || ARGV.length > 2 || tests[ARGV[0]] == nil
  STDERR.puts "Usage: check-bundle cardano-wallet-(jormungandr|byron|shelley) [RUNNER]"
  exit 1
end

wallet = ARGV[0]

# Runner is used to run windows exes under wine.
runner = ARGV.fetch(1, "")

$failed = 0

def report(cmd, status)
  res = status == 0 ? "pass" : "FAIL"
  puts "#{cmd.ljust(40)}[#{res}]"
  if status != 0 then
    $failed = 1
  end
end

cmd = "#{wallet} version"
ver, status = Open3.capture2("#{runner} #{cmd}")
report(cmd, status.to_i)

tests[wallet].each do |cmd|
  begin
    stdout_str, status = Open3.capture2("#{runner} #{cmd} --help")
    report(cmd, status.to_i)
  rescue Errno::ENOENT
    report(cmd, 1)
  end
end

exit($failed)
