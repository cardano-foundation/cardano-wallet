##
# cardano-cli cmd helper wrapper
#
class CardanoCli

  attr_reader :node_state, :socket_path

  def initialize
    @node_state = File.join(absolute_path(ENV['TESTS_NODE_DB']), CONTEXT.env)

    if is_win?
      @socket_path = '\\\\.\\pipe\\cardano-node-testnet'
    else
      @socket_path = File.join(@node_state, 'node.socket')
    end

    ENV['CARDANO_NODE_SOCKET_PATH'] = @socket_path
  end

  def build_script_address(script_file_path)
    cmd(%(cardano-cli address build \
          --payment-script-file #{script_file_path} \
          --testnet-magic #{get_protocol_magic})).gsub("\n", '')
  end

  def generate_payment_keys
    keys = {
              vkey: File.join(@node_state, 'payment.vkey'),
              skey: File.join(@node_state, 'payment.skey')
           }
    cmd(%(cardano-cli address key-gen \
           --verification-key-file #{keys[:vkey]} \
           --signing-key-file #{keys[:skey]}))
    keys
  end

  def build_payment_address(keys)
    cmd(%(cardano-cli address build \
           --payment-verification-key-file #{keys[:vkey]} \
           --testnet-magic #{get_protocol_magic})).gsub("\n", '')
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
    output = cmd(%(cardano-cli query utxo \
                    --address #{address} \
                    --testnet-magic #{get_protocol_magic}))
    # [utxo1, utxo2, ... utxoN]
    #     where utxoN = {utxo: utxoId, ix: index, amt: ada amount}
    output.partition("-" * 86).last.strip.split("\n").map do |utxo|
      utxo_arr = utxo.split(" ")
      {utxo: utxo_arr[0], ix: utxo_arr[1], amt: utxo_arr[2]}
    end
  end

  def get_protocol_params
    pparams = File.join(@node_state, 'pparams.json')
    unless File.exists?(pparams)
      log "Getting pparams.json"
      cmd(%(cardano-cli query protocol-parameters \
            --testnet-magic #{get_protocol_magic} \
            --out-file #{pparams}))
    else
      log "Using existing pparams.json" 
    end
    pparams
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
    cmd(%(cardano-cli transaction build-raw \
          --tx-in #{script_utxo} \
          --tx-out "#{target_addr}+#{50000000 - fee}" \
          --tx-in-script-file #{script_file} \
          --tx-in-datum-value 1914 \
          --tx-in-redeemer-value 123 \
          --tx-in-collateral #{collateral_utxo} \
          --tx-in-execution-units "(10, 10)" \
          --protocol-params-file #{get_protocol_params} \
          --fee #{fee} \
          --tx-out-return-collateral "#{collateral_ret_addr}+#{collateral_utxo_amt - (fee * 1.5).to_i}" \
          --tx-total-collateral #{(fee * 1.5).to_i} \
          --script-invalid \
          --babbage-era \
          --out-file #{txbody}))
    txbody
  end

  def tx_sign(txbody, keys)
    txsigned = File.join(@node_state, 'txsigned')
    cmd(%(cardano-cli transaction sign \
           --tx-body-file #{txbody} \
           --testnet-magic #{get_protocol_magic} \
           --signing-key-file #{keys[:skey]} \
           --out-file #{txsigned}))
    txsigned
  end

  def tx_submit(txsigned)
    # submit
    cmd(%(cardano-cli transaction submit \
          --tx-file #{txsigned} \
          --testnet-magic #{get_protocol_magic} ))

    # return tx id
    cmd(%(cardano-cli transaction txid --tx-file #{txsigned})).gsub("\n", '')
  end

end
