require 'bip_mnemonic'
require 'httparty'
require 'fileutils'

module Helpers
  module Utils

    def absolute_path(path)
      if path.start_with? "."
        File.join(Dir.pwd, path[1..-1])
      else
        path
      end
    end

    def get_fixture_wallet_mnemonics(type)
      fixture = ENV['TESTS_E2E_FIXTURES_FILE']
      unless File.exists? fixture
        raise "File #{fixture} does not exist! (Hint: Template fixture file can be created with 'rake fixture_wallets_template'). Make sure to feed it with mnemonics of wallets with funds and assets."
      end
      wallets = JSON.parse File.read(fixture)
      if is_linux?
        wallets["linux"][type]
      elsif is_mac?
        wallets["macos"][type]
      elsif is_win?
        wallets["windows"][type]
      else
        wallets["linux"][type]
      end
    end

    def mnemonic_sentence(word_count = 15)
      case word_count
      when 9
        bits = 96
      when 12
        bits = 128
      when 15
        bits = 164
      when 18
        bits = 196
      when 21
        bits = 224
      when 24
        bits = 256
      else
        raise "Non-supported no of words #{word_count}!"
      end
      BipMnemonic.to_mnemonic(bits: bits, language: 'english').split
    end

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

    def rm_files(path)
      FileUtils.rm_rf("#{path}/.", secure: true)
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
  end
end
