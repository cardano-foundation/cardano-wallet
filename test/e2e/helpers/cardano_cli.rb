# frozen_string_literal: true

##
# cardano-cli cmd helper wrapper
#
class CardanoCli
  attr_reader :node_state, :socket_path, :protocol_magic

  def initialize(protocol_magic)
    @node_state = File.join(absolute_path(ENV.fetch('TESTS_NODE_DB', nil)), CONTEXT.env)
    @protocol_magic = protocol_magic

    if win?
      @socket_path = '\\\\.\\pipe\\cardano-node-testnet'
    else
      @socket_path = ENV.fetch('CARDANO_NODE_SOCKET_PATH')
      # Add additional permissions to node.socket if we're in e2e docker test suite,
      # so cardano-cli can work with cardano-node from the docker container
      cmd(%(sudo chmod a+rwx #{@socket_path})) if ENV['E2E_DOCKER_RUN'] == '1'
    end
    ENV['CARDANO_NODE_SOCKET_PATH'] = @socket_path
  end

  def cli(*options)
    cmd(%(cardano-cli #{options.join(' ')}))
  end

  def build_script_address(script_file_path)
    cli('address', 'build', '--payment-script-file', script_file_path, '--testnet-magic', @protocol_magic).strip
  end

  def generate_payment_keys
    keys = {
      payment_vkey: File.join(@node_state, 'payment.vkey'),
      payment_skey: File.join(@node_state, 'payment.skey')
    }
    cli('address', 'key-gen',
        '--verification-key-file', keys[:payment_vkey],
        '--signing-key-file', keys[:payment_skey])
    keys
  end

  def build_payment_address(keys)
    cli('address', 'build',
        '--payment-verification-key-file', keys[:payment_vkey],
        '--testnet-magic', @protocol_magic).strip
  end

  ##
  # Returns simplified utxo array for an address.
  # Parse cardano-cli query utxo output, like:
  #   TxHash                                 TxIx        Amount
  # --------------------------------------------------------------------------------------
  # 4f10e314ca4f71031ae2f801638d1671571bc0fa811bd59520b34d3e68ae5344     0        10000000 lovelace + TxOutDatumNone
  # f8e12cf50ebf8b0a3d87869f8ca31ed1a95acc77dcc6007997ea97cb9f5a24cd     0        10000000 lovelace + TxOutDatumNone
  #
  # into:
  # @return [Array] - [{utxo: utxoId, ix: index, amt: ada amount}, ...]
  def get_utxos(address)
    output = cli('query', 'utxo',
                 '--address', address,
                 '--testnet-magic', @protocol_magic)
    # [utxo1, utxo2, ... utxoN]
    #     where utxoN = {utxo: utxoId, ix: index, amt: ada amount}
    output.partition('-' * 86).last.strip.split("\n").map do |utxo|
      utxo_arr = utxo.split
      { utxo: utxo_arr[0], ix: utxo_arr[1], amt: utxo_arr[2] }
    end
  end

  def get_protocol_params_to_file(file = 'pparams.json')
    pparams_path = File.join(@node_state, file)
    cli('query', 'protocol-parameters',
        '--testnet-magic', @protocol_magic,
        '--out-file', pparams_path)
    pparams_path
  end

  def protocol_params
    pparams = cli('query', 'protocol-parameters',
                  '--testnet-magic', @protocol_magic)
    JSON.parse(pparams)
  end

  ##
  # Build (using trasaction build-raw) an invalid transaction that is
  # spending from script address and always fails. Using collateral return option.
  def tx_build_raw_always_fails(script_file,
                                script_utxo,
                                collateral_utxo,
                                collateral_utxo_amt,
                                fee,
                                target_addr,
                                collateral_ret_addr)
    txbody = File.join(@node_state, 'txbody')
    cli('transaction', 'build-raw',
        '--tx-in', script_utxo,
        '--tx-out', "#{target_addr}+#{50_000_000 - fee}",
        '--tx-in-script-file', script_file,
        '--tx-in-datum-value', 1914,
        '--tx-in-redeemer-value', 123,
        '--tx-in-collateral', collateral_utxo,
        '--tx-in-execution-units', '"(10, 10)"',
        '--protocol-params-file', get_protocol_params_to_file,
        '--fee', fee,
        '--tx-out-return-collateral', "#{collateral_ret_addr}+#{collateral_utxo_amt - (fee * 1.5).to_i}",
        '--tx-total-collateral', (fee * 1.5).to_i,
        '--script-invalid',
        '--babbage-era',
        '--out-file', txbody)
    txbody
  end

  def tx_build(*options)
    txbody = File.join(@node_state, 'txbody')
    cli('transaction', 'build',
        '--babbage-era',
        '--testnet-magic', @protocol_magic,
        '--out-file', txbody,
        *options)
    txbody
  end

  def tx_sign(txbody, keys)
    txsigned = File.join(@node_state, 'txsigned')
    signing_keys = keys.filter { |k, _| k.to_s.end_with?('_skey') }.map { |_, v| "--signing-key-file #{v}" }
    cli('transaction', 'sign',
        '--tx-body-file', txbody,
        '--testnet-magic', @protocol_magic,
        signing_keys.join(' '), # --signing-key-file key1 --signing-key-file key2 ...
        '--out-file', txsigned)
    txsigned
  end

  # @return [String] - tx id
  def tx_submit(txsigned)
    # submit
    cli('transaction', 'submit',
        '--tx-file', txsigned,
        '--testnet-magic', @protocol_magic)

    # return tx id
    cli('transaction', 'txid',
        '--tx-file', txsigned).strip
  end

  def policy_id(script_file)
    cli('transaction', 'policyid',
        '--script-file', script_file).strip
  end
end
