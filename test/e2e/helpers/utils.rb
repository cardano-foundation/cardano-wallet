require 'bip_mnemonic'
require 'httparty'
require 'fileutils'

module Helpers
  module Utils
    def log(message)
      puts "[#{Time.now}] #{message}"
    end

    def cmd(cmd, display_result = false)
      cmd.gsub(/\s+/, ' ')
      res = `#{cmd}`
      log cmd if display_result
      log res if display_result
      res
    end

    ##
    # Generate Byron address from mnemonic sentence and derivation path
    # $ cat mnemonics \
    #   | cardano-address key from-recovery-phrase Byron > root.prv
    #
    # $ cat root.prv \
    #   | cardano-address key child 14H/42H | tee addr.prv \
    #   | cardano-address key public --with-chain-code \
    #   | cardano-address address bootstrap --root $(cat root.prv | cardano-address key public --with-chain-code) \
    #       --network-tag testnet 14H/42H
    def cardano_address_get_byron_addr(mnemonics, derivation_path)
      root = cmd(%(echo #{mnemonics.join(' ')} | cardano-address key from-recovery-phrase Byron | cardano-address key public --with-chain-code)).gsub("\n", '')
      cmd(%(echo #{mnemonics.join(' ')} \
         | cardano-address key from-recovery-phrase Byron \
         | cardano-address key child #{derivation_path} \
         | cardano-address key public --with-chain-code \
         | cardano-address address bootstrap \
         --root #{root} \
         --network-tag testnet #{derivation_path}
         )).gsub("\n", '')
    end

    def cardano_address_get_acc_xpub(mnemonics, derivation_path, hex = true, wallet_type = "Shared", chain_code = "--with-chain-code")
      cmd(%(echo #{mnemonics.join(' ')} \
         | cardano-address key from-recovery-phrase #{wallet_type} \
         | cardano-address key child #{derivation_path} \
         | cardano-address key public #{chain_code} #{" | bech32" if hex})).gsub("\n", '')
    end

    def bech32_to_base16(key)
      cmd(%(echo #{key} | bech32)).gsub("\n", '')
    end

    def hex_to_bytes(s)
      s.scan(/../).map { |x| x.hex.chr }.join
    end

    def binary_to_hex(binary_as_string)
      "%02x" % binary_as_string.to_i(2)
    end

    ##
    # encode string asset_name to hex representation
    def asset_name(asset_name)
      asset_name.unpack("H*").first
    end

    def absolute_path(path)
      if path.start_with? "."
        File.join(Dir.pwd, path[1..-1])
      else
        path
      end
    end

    # Get wallet mnemonics from fixures file
    # @param kind [Symbol] :fixture or :target (fixture wallet with funds or target wallet)
    # @param type [Symbol] wallet type = :shelley, :shared, :icarus, :random
    def get_fixture_wallet_mnemonics(kind, type)
      fixture = ENV['TESTS_E2E_FIXTURES_FILE']
      unless File.exists? fixture
        raise "File #{fixture} does not exist! (Hint: Template fixture file can be created with 'rake fixture_wallets_template'). Make sure to feed it with mnemonics of wallets with funds and assets."
      end
      wallets = JSON.parse File.read(fixture)
      k = kind.to_s
      t = type.to_s
      if is_linux?
        wallets["linux"][k][t]
      elsif is_mac?
        wallets["macos"][k][t]
      elsif is_win?
        wallets["windows"][k][t]
      else
        wallets["linux"][k][t]
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
      file ||= File.basename(url)
      resp = HTTParty.get(url)
      File.binwrite(file, resp.body)
      log "#{url} -> #{resp.code}"
    end

    def mk_dir(path)
      FileUtils.mkdir_p(path)
    end

    def rm_files(path)
      FileUtils.rm_rf(path, secure: true)
    end

    def mv(src, dst)
      FileUtils.mv(src, dst, force: true)
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

    def get_latest_binary_url(pr = nil)
      if is_linux?
        os = "linux.musl.cardano-wallet-linux64"
      end
      if is_mac?
        os = "macos.intel.cardano-wallet-macos-intel"
      end
      if is_win?
        os = "linux.windows.cardano-wallet-win64"
      end
      if pr
        "https://hydra.iohk.io/job/Cardano/cardano-wallet-pr-#{pr}/#{os}/latest/download-by-type/file/binary-dist"
      else
        "https://hydra.iohk.io/job/Cardano/cardano-wallet/#{os}/latest/download-by-type/file/binary-dist"
      end
    end

    ##
    # Latest Cardano configs
    def get_latest_configs_base_url(env)
      case env
      when 'mainnet', 'testnet', /vasil-*/
        "https://book.world.dev.cardano.org/environments/#{env}/"
      else
        "https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/#{env}-"
      end
    end

    ##
    # Latest node-db snapshot updated at the end of every epoch
    def get_latest_node_db_url(env)
      raise "Unsupported env, supported are: 'mainnet' or 'testnet'" if (env != 'testnet') && (env != 'mainnet')
      case env
      when 'testnet'
        "https://updates-cardano-testnet.s3.amazonaws.com/cardano-node-state/db-testnet.tar.gz"
      when 'mainnet'
        "https://update-cardano-mainnet.iohk.io/cardano-node-state/db-mainnet.tar.gz"
      end
    end
  end
end

##
# extend String class with hexdump methods
class String
  def cbor_to_hex
    bytes.map { |x| "%02x" % x }.join
  end
end
