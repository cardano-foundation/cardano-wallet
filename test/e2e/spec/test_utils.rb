require 'httparty'

def wget(url, file = nil)
  file = File.basename(url) unless file
  resp = HTTParty.get(url)
  open(file, "wb") do |file|
    file.write(resp.body)
  end
  puts "#{url} -> #{resp.code}"
end

def mk_dir(path)
  Dir.mkdir(path) unless File.exists?(path)
end

def is_win?
  RUBY_PLATFORM =~ /cygwin|mswin|mingw|bccwin|wince|emx/
end

def is_linux?
  RUBY_PLATFORM =~ /linux/
end

def is_mac?
  RUBY_PLATFORM =~ /darwin/
end

def get_latest_binary_url
  if is_linux?
    os = "linux"
  end
  if is_mac?
    os = "macos"
  end
  if is_win?
    os = "win"
  end

  "https://hydra.iohk.io/job/Cardano/cardano-wallet/cardano-wallet-#{os}64/latest/download-by-type/file/binary-dist"
end

def get_latest_configs_base_url
  "https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1"
end
