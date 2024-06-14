# frozen_string_literal: true

require 'httparty'
require 'fileutils'

module Helpers
  # general utility methods
  module Utils
    def log(message)
      puts "[#{Time.now}] #{message}"
    end

    def cmd(cmd, display_result: false)
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
      root = cmd(%(echo #{mnemonics.join(' ')} | cardano-address key from-recovery-phrase Byron | cardano-address key public --with-chain-code)).gsub(
        "\n", ''
      )
      cmd(%(echo #{mnemonics.join(' ')} \
         | cardano-address key from-recovery-phrase Byron \
         | cardano-address key child #{derivation_path} \
         | cardano-address key public --with-chain-code \
         | cardano-address address bootstrap \
         --root #{root} \
         --network-tag testnet #{derivation_path}
         )).gsub("\n", '')
    end

    def cardano_address_get_acc_xpub(mnemonics, derivation_path, wallet_type = 'Shared',
                                     chain_code = '--with-chain-code', hex: false)
      cmd(%(echo #{mnemonics.join(' ')} \
         | cardano-address key from-recovery-phrase #{wallet_type} \
         | cardano-address key child #{derivation_path} \
         | cardano-address key public #{chain_code} #{' | bech32' if hex})).gsub("\n", '')
    end

    def bech32_to_base16(key)
      cmd(%(echo #{key} | bech32)).gsub("\n", '')
    end

    def hex_to_bytes(str)
      str.scan(/../).map { |x| x.hex.chr }.join
    end

    def binary_to_hex(binary_as_string)
      format('%02x', binary_as_string.to_i(2))
    end

    ##
    # encode string asset_name to hex representation
    def asset_name(asset_name)
      asset_name.unpack1('H*')
    end

    def absolute_path(path)
      if path.start_with? '.'
        File.join(Dir.pwd, path[1..])
      else
        path
      end
    end

    # Get wallet mnemonics/payment template/delegation template from fixures file
    # @param kind [Symbol] :fixture or :target (fixture wallet with funds or target wallet)
    # @param wallet_type [Symbol] wallet type = :shelley, :shared, :icarus, :random
    # @param info_type [Symbol] wallet type = :mnemonics, :payment_template, :delegation_template
    def get_fixture_wallet(kind, wallet_type, info_type = :mnemonics)
      fixture = ENV.fetch('TESTS_E2E_FIXTURES_FILE', nil)
      log "Using fixture file: #{fixture}"
      raise "File #{fixture} does not exist! (Hint: Template fixture file can be created with 'rake fixture_wallets_template'). Make sure to feed it with mnemonics of wallets with funds and assets." unless File.exist? fixture

      wallets = from_json(fixture)
      log "Using #{kind} wallet of type #{wallet_type} with #{info_type} info."
      log "Wallets: #{wallets}"
      if linux?
        r = wallets[:linux][kind][wallet_type][info_type]
        log "Wallet info: #{r}"
        r
      elsif mac?
        wallets[:macos][kind][wallet_type][info_type]
      elsif win?
        wallets[:windows][kind][wallet_type][info_type]
      else
        raise 'Unsupported platform!'
      end
    end

    def from_json(file)
      JSON.parse(File.read(file), { symbolize_names: true })
    end

    def wget(url, file = nil)
      file ||= File.basename(url)
      resp = HTTParty.get(url)
      File.binwrite(file, resp.body)
      log "#{url} -> #{resp.code}"
    end

    def unzip_artifact(target_file, target_dir)
      log "Unzipping #{target_file} to #{target_dir}"
      if win?
        cmd "unzip -o #{target_file} -d #{target_dir}"
        cmd "del \\f #{target_file}"
      else
        cmd "tar -xvf #{target_file}"
        cmd "cp -r cardano-wallet-*/* #{target_dir}"
        cmd "rm -rf cardano-wallet-*/ #{target_file}"
      end
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

    def win?
      RUBY_PLATFORM =~ /cygwin|mswin|mingw|bccwin|wince|emx/
    end

    def linux?
      RUBY_PLATFORM =~ /linux/
    end

    def mac?
      RUBY_PLATFORM =~ /darwin/
    end

    ##
    # Latest Cardano configs
    def get_latest_configs_base_url(env)
      envs = %w[preview preprod].freeze
      raise "Env '#{env}' is not supported, supported are: #{envs}" unless envs.include? env

      "https://book.world.dev.cardano.org/environments/#{env}/"
    end

    ##
    # Latest node-db snapshot updated at the end of every epoch
    def get_latest_node_db_url(env)
      raise "Unsupported env, supported are: 'mainnet' or 'testnet'" if (env != 'testnet') && (env != 'mainnet')

      case env
      when 'testnet'
        'https://updates-cardano-testnet.s3.amazonaws.com/cardano-node-state/db-testnet.tar.gz'
      when 'mainnet'
        'https://update-cardano-mainnet.iohk.io/cardano-node-state/db-mainnet.tar.gz'
      end
    end

    ##
    # Get protocol magic from byron-genesis.json corresponding to particular env
    def get_protocol_magic(env)
      config = File.join(absolute_path(ENV.fetch('CARDANO_NODE_CONFIGS', nil)), env)
      byron_genesis = JSON.parse(File.read(File.join(config, 'byron-genesis.json')))
      byron_genesis['protocolConsts']['protocolMagic'].to_i
    end

    def base64?(value)
      value.is_a?(String) && Base64.strict_encode64(Base64.decode64(value)) == value
    end

    def base16?(value)
      value.is_a?(String) && value.match?(/^[[:xdigit:]]+$/)
    end
  end
end

##
# extend String class with hexdump methods
class String
  def cbor_to_hex
    bytes.map { |x| format('%02x', x) }.join
  end
end
