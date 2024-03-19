
############################################################################
#!/usr/bin/env ruby
# Checks that every executable required in the release package is
# present and works.
# On Linux it checks if executables are statically linked.
# On macOS it checks that there are no /nix/store dylibs.
############################################################################

require 'open3'

tests = {
  "cardano-wallet.exe" => [ "cardano-node", "cardano-cli", "bech32", "cardano-address" ], "cardano-wallet" => [ "cardano-node", "cardano-cli", "bech32", "cardano-address" ]
}

if ARGV.length < 1 || ARGV.length > 2 || tests[ARGV[0]] == nil
  STDERR.puts "Usage: check-bundle cardano-wallet [RUNNER]"
  STDERR.puts ARGV[0]
  STDERR.puts ARGV[1]
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
begin
  ver, status = Open3.capture2("#{runner} #{cmd}")
  report(cmd, status.to_i)
rescue Errno::ENOENT
  report(cmd, 1)
end

tests[wallet].unshift(wallet).each do |cmd|
  begin
    stdout_str, status = Open3.capture2("#{runner} #{cmd} --help")
    report(cmd, status.to_i)
  rescue Errno::ENOENT
    report(cmd, 1)
    next
  end

  if /darwin/ =~ RUBY_PLATFORM then
    system("! otool -L `type -p #{cmd}` | sed 1d | grep nix");
    report("#{cmd} is free of /nix/store", $?.exitstatus)
  elsif /linux/ =~ RUBY_PLATFORM then
    system("! ldd `type -p #{cmd}` > /dev/null");
    report("#{cmd} is static linked", $?.exitstatus)
  end
end

exit($failed)
