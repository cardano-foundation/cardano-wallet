# frozen_string_literal: true

RSpec.describe 'Cardano Wallet E2E tests', :all, :e2e do
  before(:all) do
    # shelley wallets
    @wid = create_fixture_wallet(:shelley)
    @target_id = create_target_wallet(:shelley)

    # byron wallets
    @wid_rnd = create_fixture_wallet(:random)
    @wid_ic = create_fixture_wallet(:icarus)

    @nightly_byron_wallets = [@wid_rnd, @wid_ic]
    @nightly_shelley_wallets = [@wid, @target_id]
    wait_for_all_shelley_wallets(@nightly_shelley_wallets)
    wait_for_all_byron_wallets(@nightly_byron_wallets)
  end

  after(:each) do
    teardown
  end

  after(:all) do
    SHELLEY.stake_pools.quit(@target_id, PASS)
  end

  describe 'Regressions' do
    it 'ADP-2523 - Make sure there are no null values in the response', :adp_2523 do
      pools = SHELLEY.stake_pools
      l = pools.list({ stake: 1000 })
      expect(l).to be_correct_and_respond 200
      expect(l.length).to be > 0
      expect(l.to_s).not_to include 'null'
    end

    it 'ADP-2666 - Tx history is available after receiving token from minting tx made using reference script (Plutus script)', :adp_2666 do
      ##
      # This test is to reproduce a bug where tx history was not available
      # after receiving token from minting tx made using reference script.
      # Reproduction steps:
      # 1. [cardano-cli] Create an address and fund it with ada
      # 2. [cardano-cli] Submit transaction to the address setting utxo for collateral and reference script utxo (Plutus script)
      # 3. [cardano-cli] Submit minting transaction using reference script sending minted tokens to wallet address
      # 4. [cardano-wallet] Check that tx history is available after receiving token from minting tx made using reference script
      # 5. [cardano-wallet] Send token back to the address

      # 1. [cardano-cli] Create an address and fund it with ada
      payment_keys = CARDANO_CLI.generate_payment_keys
      payment_address = CARDANO_CLI.build_payment_address(payment_keys)
      init_amt = 20_000_000
      tx = construct_sign_submit(@wid, payment_payload(init_amt, payment_address))
      wait_for_tx_in_ledger(@wid, tx.last['id'])

      # 2. [cardano-cli] Submit transaction to the address setting utxo for collateral and reference script utxo
      init_utxo = CARDANO_CLI.get_utxos(payment_address).first
      txbody = CARDANO_CLI.tx_build("--tx-in #{init_utxo[:utxo]}##{init_utxo[:ix]}",
                                    "--tx-out #{payment_address}+10000000", # will be a reference script utxo (#0)
                                    "--tx-out-reference-script-file #{get_plutus_file_path('anyone-can-mint.plutus')}",
                                    "--tx-out #{payment_address}+3000000", # will be a collateral utxo (#1)
                                    "--change-address #{payment_address}") # will be a regular utxo (#2)

      txsigned = CARDANO_CLI.tx_sign(txbody, payment_keys)
      txid = CARDANO_CLI.tx_submit(txsigned)

      eventually 'Tx is in ledger' do
        CARDANO_CLI.get_utxos(payment_address).to_s.include?(txid)
      end

      # 3. [cardano-cli] Submit minting transaction using reference script sending minted tokens to wallet address

      wallet_id = @target_id
      txs = SHELLEY.transactions.list(wallet_id)
      expect(txs).to be_correct_and_respond 200

      src_utxos = CARDANO_CLI.get_utxos(payment_address)
      address = SHELLEY.addresses.list(wallet_id).first['id']
      policy_id = CARDANO_CLI.policy_id(get_plutus_file_path('anyone-can-mint.plutus'))
      hex_asset_name = asset_name('ReferencePlutusScriptAsset')
      txbody2 = CARDANO_CLI.tx_build("--tx-in #{src_utxos[2][:utxo]}##{src_utxos[2][:ix]}",
                                     "--tx-in-collateral #{src_utxos[1][:utxo]}##{src_utxos[1][:ix]}",
                                     "--mint-tx-in-reference #{src_utxos[0][:utxo]}##{src_utxos[0][:ix]}",
                                     "--tx-out \"#{address}+2000000+1 #{policy_id}.#{hex_asset_name}\"",
                                     "--mint \"1 #{policy_id}.#{hex_asset_name}\"",
                                     '--mint-plutus-script-v2',
                                     "--mint-reference-tx-in-redeemer-file #{get_plutus_file_path('42.redeemer')}",
                                     "--policy-id #{policy_id}",
                                     "--change-address #{payment_address}",
                                     "--protocol-params-file #{CARDANO_CLI.get_protocol_params_to_file}")

      txsigned2 = CARDANO_CLI.tx_sign(txbody2, payment_keys)
      txid2 = CARDANO_CLI.tx_submit(txsigned2)

      eventually 'Minting Tx with reference Plutus script is in ledger' do
        CARDANO_CLI.get_utxos(payment_address).to_s.include?(txid2)
      end

      # 4. [cardano-wallet] Check that tx history is available after receiving token from minting tx made using reference script
      txs = SHELLEY.transactions.list(wallet_id)
      expect(txs).to be_correct_and_respond 200

      tx_details = SHELLEY.transactions.get(wallet_id, txid2)
      tx_inputs(tx_details, present: true)
      tx_outputs(tx_details, present: true)
      tx_direction(tx_details, 'incoming')
      tx_script_validity(tx_details, 'valid')
      tx_status(tx_details, 'in_ledger')
      tx_collateral(tx_details, present: true)
      tx_collateral_outputs(tx_details, present: true)
      tx_metadata(tx_details, nil)
      tx_deposits(tx_details, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx_details, present: false)
      tx_has_mint_or_burn(tx_details, mint: true, burn: false)
      expect(tx_details['mint']['tokens'].first['policy_script']['script_type']).to eq 'reference script'
      tx_extra_signatures(tx_details, present: true)
      tx_script_integrity(tx_details, present: true)
      tx_validity_interval_default(tx_details)
      tx_certificates(tx_details, present: false)

      # 5. [cardano-wallet] Send token back to the address
      payment = [{ 'address' => payment_address,
                   'amount' => { 'quantity' => 0, 'unit' => 'lovelace' },
                   'assets' => [{ 'policy_id' => policy_id,
                                  'asset_name' => hex_asset_name,
                                  'quantity' => 1 }] }]
      tx = construct_sign_submit(wallet_id, payment)
      wait_for_tx_in_ledger(wallet_id, tx.last['id'])
    end

    it 'ADP-2666 - Tx history is available after receiving token from minting tx made using reference script (Simple script)', :adp_2666 do
      ##
      # This test is to reproduce a bug where tx history was not available
      # after receiving token from minting tx made using reference script (using simple script a.k.a. native script).
      # Reproduction steps:
      # 1. [cardano-cli] Create an address and fund it with ada
      # 2. [cardano-cli] Submit transaction to the address setting utxo for collateral and reference script utxo
      # 3. [cardano-cli] Submit minting transaction using reference script sending minted tokens to wallet address
      # 4. [cardano-wallet] Check that tx history is available after receiving token from minting tx made using reference script
      # 5. [cardano-wallet] Send token back to the address

      # 1. [cardano-cli] Create an address and fund it with ada
      payment_keys = CARDANO_CLI.generate_payment_keys
      payment_address = CARDANO_CLI.build_payment_address(payment_keys)
      init_amt = 10_000_000
      tx = construct_sign_submit(@wid, payment_payload(init_amt, payment_address))
      wait_for_tx_in_ledger(@wid, tx.last['id'])

      # 2. [cardano-cli] Submit transaction to the address setting utxo for collateral and reference script utxo
      init_utxo = CARDANO_CLI.get_utxos(payment_address).first
      txbody = CARDANO_CLI.tx_build("--tx-in #{init_utxo[:utxo]}##{init_utxo[:ix]}",
                                    "--tx-out #{payment_address}+5000000", # will be a reference script utxo (#0)
                                    "--tx-out-reference-script-file #{get_simple_scripts_file_path('policy.script')}",
                                    "--change-address #{payment_address}") # will be a regular utxo (#1)

      txsigned = CARDANO_CLI.tx_sign(txbody, payment_keys)
      txid = CARDANO_CLI.tx_submit(txsigned)

      eventually 'Tx is in ledger' do
        CARDANO_CLI.get_utxos(payment_address).to_s.include?(txid)
      end

      # 3. [cardano-cli] Submit minting transaction using reference script sending minted tokens to wallet address
      wallet_id = @target_id
      txs = SHELLEY.transactions.list(wallet_id)
      expect(txs).to be_correct_and_respond 200

      src_utxos = CARDANO_CLI.get_utxos(payment_address)
      address = SHELLEY.addresses.list(wallet_id).first['id']
      policy_id = CARDANO_CLI.policy_id(get_simple_scripts_file_path('policy.script'))
      hex_asset_name = asset_name('ReferenceSimpleScriptAsset')
      txbody2 = CARDANO_CLI.tx_build("--tx-in #{src_utxos[1][:utxo]}##{src_utxos[1][:ix]}",
                                     '--witness-override 2',
                                     "--simple-minting-script-tx-in-reference #{src_utxos[0][:utxo]}##{src_utxos[0][:ix]}",
                                     "--tx-out \"#{address}+2000000+1 #{policy_id}.#{hex_asset_name}\"",
                                     "--mint \"1 #{policy_id}.#{hex_asset_name}\"",
                                     "--policy-id #{policy_id}",
                                     "--change-address #{payment_address}",
                                     "--protocol-params-file #{CARDANO_CLI.get_protocol_params_to_file}")

      payment_keys[:policy_skey] = get_simple_scripts_file_path('policy.skey')
      txsigned2 = CARDANO_CLI.tx_sign(txbody2, payment_keys)
      txid2 = CARDANO_CLI.tx_submit(txsigned2)

      eventually 'Minting Tx with reference Simple script is in ledger' do
        CARDANO_CLI.get_utxos(payment_address).to_s.include?(txid2)
      end

      # 4. [cardano-wallet] Check that tx history is available after receiving token from minting tx made using reference script
      txs = SHELLEY.transactions.list(wallet_id)
      expect(txs).to be_correct_and_respond 200

      tx_details = SHELLEY.transactions.get(wallet_id, txid2)
      tx_inputs(tx_details, present: true)
      tx_outputs(tx_details, present: true)
      tx_direction(tx_details, 'incoming')
      tx_script_validity(tx_details, 'valid')
      tx_status(tx_details, 'in_ledger')
      tx_collateral(tx_details, present: false)
      tx_collateral_outputs(tx_details, present: false)
      tx_metadata(tx_details, nil)
      tx_deposits(tx_details, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx_details, present: false)
      tx_has_mint_or_burn(tx_details, mint: true, burn: false)
      expect(tx_details['mint']['tokens'].first['policy_script']['script_type']).to eq 'reference script'
      tx_extra_signatures(tx_details, present: false)
      tx_script_integrity(tx_details, present: false)
      tx_validity_interval_default(tx_details)
      tx_certificates(tx_details, present: false)

      # 5. [cardano-wallet] Send token back to the address
      payment = [{ 'address' => payment_address,
                   'amount' => { 'quantity' => 0, 'unit' => 'lovelace' },
                   'assets' => [{ 'policy_id' => policy_id,
                                  'asset_name' => hex_asset_name,
                                  'quantity' => 1 }] }]
      tx = construct_sign_submit(wallet_id, payment)
      wait_for_tx_in_ledger(wallet_id, tx.last['id'])
    end
  end

  describe 'Collateral return', :collateral do
    it 'AlwaysFails.plutus with collateral return to the wallet' do
      ##
      # This test is trying to spend utxo from a script address
      # that will always fail.
      # Script: https://github.com/input-output-hk/cardano-node/blob/master/scripts/plutus/scripts/v2/always-fails.plutus
      #
      # The spending transaction sets:
      # - collateral return output to be sent to wallet address
      # - aims to spend ADA also to the wallet address
      #
      # Because the script will fail we expect:
      # - collateral return output to be send to the wallet address
      # - wallet to show balance correctly
      # - wallet to be able to spend this collateral output in the subsequent transaction
      #
      # We are trying to spend pre-created UTxO from the script address,
      # which was created as follows using fixtures/alwaysfails.plutus:
      #
      # export NETWORK_ID="--testnet-magic 2"
      # cardano-cli address build --payment-script-file alwaysfails.plutus $NETWORK_ID > AlwaysFails.addr
      # cardano-cli transaction hash-script-data --script-data-value 1914 > datumhash
      # cardano-cli transaction build  \
      # 	--babbage-era  \
      # 	$NETWORK_ID \
      # 	--tx-in "ab08ccdf5c62ad8008d0ac165b68ff714b88de19235a9bd65c731fc264125daf#0"  \
      # 	--tx-out $(<AlwaysFails.addr)+50000000  \
      # 	--tx-out-datum-hash $(<datumhash) \
      # 	--change-address $(cat payment.addr) \
      # --protocol-params-file protocol.json  \
      # --out-file body.tx
      #
      #  cardano-cli transaction sign \
      #    --tx-body-file body.tx \
      #    $NETWORK_ID \
      #    --signing-key-file payment.skey \
      #    --out-file signed.tx
      #
      # cardano-cli transaction submit --tx-file signed.tx $NETWORK_ID
      #
      # Therefore it needs to be done once for each network we want to run our test against.

      case CONTEXT.env
      when 'vasil-dev'
        script_utxo = 'ce149a5dea4b09d1717ffbe79f8e46ddd9bf0401e95a69502b71f792982b5013#1'
      when 'testnet'
        script_utxo = '54b4e4e34a022424e441b00d8a73e9aaef71b3c63084e76246d326074c5d3756#1'
      when 'preview'
        script_utxo = '0c07395aed88bdddc6de0518d1462dd0ec7e52e1e3a53599f7cdb24dc80237f8#1'
      when 'preprod'
        script_utxo = '6d2174d3956d8eb2b3e1e198e817ccf1332a599d5d7320400bfd820490d706be#0'
      else
        skip %(
                This test cannot be executed on '#{ENV.fetch('NETWORK', nil)}' yet!
                Follow instructions in test description to prepare alwaysfails.plutus UTxO for it.
              )
      end

      # Create payment address
      payment_keys = CARDANO_CLI.generate_payment_keys
      payment_address = CARDANO_CLI.build_payment_address(payment_keys)

      # Fund payment address to be used as collateral utxo
      collateral_amt = 10_000_000
      tx = construct_sign_submit(@wid, payment_payload(collateral_amt, payment_address))
      wait_for_tx_in_ledger(@wid, tx.last['id'])
      collateral_utxo = CARDANO_CLI.get_utxos(payment_address).last
      # Try to spend from alwaysfails.plutus address
      target_address = SHELLEY.addresses.list(@target_id)[0]['id']
      target_before = get_shelley_balances(@target_id)
      explicit_fee = 2_000_000 # setting fee explicitely because we build tx raw
      txbody = CARDANO_CLI.tx_build_raw_always_fails(get_plutus_file_path('alwaysfails.plutus'),
                                                     script_utxo,
                                                     "#{collateral_utxo[:utxo]}##{collateral_utxo[:ix]}",
                                                     collateral_amt,
                                                     explicit_fee,
                                                     target_address,
                                                     target_address) # collateral_ret_addr
      txsigned = CARDANO_CLI.tx_sign(txbody, payment_keys)
      txid = CARDANO_CLI.tx_submit(txsigned)
      wait_for_tx_in_ledger(@target_id, txid)

      # Make sure tx properly displays collateral, collateral return and script_validity
      tx = SHELLEY.transactions.get(@target_id, txid)
      # collateral return amount to be returned is:
      #  collateral_ret_amt = collateral_amt - calculated_total_collateral_amt
      #  (calculated_total_collateral_amt = 150% * fee)
      collateral_ret_amt = collateral_amt - (explicit_fee * 1.5).to_i
      collateral = [{ 'id' => collateral_utxo[:utxo], 'index' => collateral_utxo[:ix].to_i }]
      collateral_outputs = [{ 'address' => target_address,
                              'amount' => { 'quantity' => collateral_ret_amt, 'unit' => 'lovelace' },
                              'assets' => [] }]
      expect(tx['collateral']).to eq collateral
      expect(tx['collateral_outputs']).to eq collateral_outputs
      expect(tx['script_validity']).to eq 'invalid'

      # Make sure balance is correct (+collateral_ret_amt)
      target_after = get_shelley_balances(@target_id)
      expect(target_after['available']).to eq(target_before['available'] + collateral_ret_amt)

      # Make sure you can spend collateral return output from the wallet
      tx = construct_sign_submit(@target_id, payment_payload(6_500_000, payment_address))
      wait_for_tx_in_ledger(@target_id, tx.last['id'])
    end
  end

  describe 'E2E Balance -> Sign -> Submit' do
    def run_script(script, payload)
      tx_balanced, tx_signed, tx_submitted = balance_sign_submit(@wid, payload)
      tx_id = tx_submitted['id']

      eventually "#{script} is in ledger" do
        tx = SHELLEY.transactions.get(@wid, tx_id)
        tx.code == 200 && tx['status'] == 'in_ledger'
      end

      { tx_id: tx_id,
        tx_unbalanced: SHELLEY.transactions.decode(@wid, payload['transaction']).parsed_response,
        tx_balanced: SHELLEY.transactions.decode(@wid, tx_balanced['transaction']).parsed_response,
        tx_signed: SHELLEY.transactions.decode(@wid, tx_signed['transaction']).parsed_response }
    end

    def run_contract(contract_setup, scripts)
      # Contract setup
      payload = get_plutus_tx(contract_setup)
      r = run_script(contract_setup, payload)
      tx_id = r[:tx_id]

      # Run Plutus contract
      scripts.each do |s|
        payload = get_templated_plutus_tx(s, { transactionId: tx_id })
        r = run_script(s, payload)
        tx_id = r[:tx_id]
      end
    end

    before(:all) do
      log 'Making transaction with 10 pure ADA inputs to the wallet to make sure there is collateral'
      amt = 10_000_000
      address = SHELLEY.addresses.list(@wid)[0]['id']
      payment = []
      10.times do
        payment << { address: address, amount: { quantity: amt, unit: 'lovelace' } }
      end
      _, _, tx_submitted = construct_sign_submit(@wid, payment)
      tx_id = tx_submitted['id']
      wait_for_tx_in_ledger(@wid, tx_id)
    end

    it 'cannot balance on empty wallet' do
      wid = create_shelley_wallet
      payload = get_plutus_tx 'ping-pong_1.json'
      tx_balanced = SHELLEY.transactions.balance(wid, payload)
      expect(tx_balanced).to be_correct_and_respond 403
      expect(tx_balanced.to_s).to include 'not_enough_money'
    end

    it 'ping-pong' do
      init_src = get_shelley_balances(@wid)
      contract_setup = 'ping-pong_1.json'
      script = 'ping-pong_2.json'

      # run contract setup
      payload = get_plutus_tx(contract_setup)
      r = run_script(contract_setup, payload)
      # verify that decoded balanced tx is the same as signed tx (modulo witness_count)
      r_balanced = r[:tx_balanced].clone
      r_signed = r[:tx_signed].clone
      r_balanced.delete('witness_count')
      r_signed.delete('witness_count')
      expect(r_balanced).to eq r_signed

      # verify witness count
      expect(r[:tx_balanced]['witness_count']['verification_key']).to eq 0
      expect(r[:tx_signed]['witness_count']['verification_key']).to be >= 1

      # verify wallet balance decreases as expected after transaction (by fee + amt)
      fee = r[:tx_balanced]['fee']['quantity']
      amt = get_sent_amt(r[:tx_balanced]['outputs'])
      src_after = get_shelley_balances(@wid)
      expect(src_after['total']).to eq(init_src['total'] - fee - amt)

      # examine tx history
      tx1 = SHELLEY.transactions.get(@wid, r[:tx_id])
      tx_inputs(tx1, present: true)
      tx_outputs(tx1, present: true)
      tx_direction(tx1, 'outgoing')
      tx_script_validity(tx1, 'valid')
      tx_status(tx1, 'in_ledger')
      tx_collateral(tx1, present: false)
      tx_collateral_outputs(tx1, present: false)
      tx_metadata(tx1, nil)
      tx_deposits(tx1, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx1, present: false)
      tx_mint_burn(tx1, mint: [], burn: [])
      tx_extra_signatures(tx1, present: true)
      tx_script_integrity(tx1, present: true)
      tx_validity_interval_default(tx1)
      tx_certificates(tx1, present: false)

      # run ping-pong_2
      src_before2 = get_shelley_balances(@wid)
      payload2 = get_templated_plutus_tx(script, { transactionId: r[:tx_id] })
      r2 = run_script(script, payload2)

      # verify that decoded balanced tx is the same as signed tx (modulo witness_count)
      r2_balanced = r2[:tx_balanced].clone
      r2_signed = r2[:tx_signed].clone
      r2_balanced.delete('witness_count')
      r2_signed.delete('witness_count')
      expect(r2_balanced).to eq r2_signed

      # verify witness count
      expect(r2[:tx_balanced]['witness_count']['verification_key']).to eq 0
      expect(r2[:tx_signed]['witness_count']['verification_key']).to be >= 1

      fee2 = r2[:tx_balanced]['fee']['quantity']

      # verify balance decreases as expected after transaction
      # ping-pong_2 spends from external utxo, so wallet balance decreases only by fee2
      src_after2 = get_shelley_balances(@wid)
      expect(src_after2['total']).to eq(src_before2['total'] - fee2)

      # examine tx history
      tx2 = SHELLEY.transactions.get(@wid, r2[:tx_id])
      tx_inputs(tx2, present: true)
      tx_outputs(tx2, present: true)
      tx_direction(tx2, 'outgoing')
      tx_script_validity(tx2, 'valid')
      tx_status(tx2, 'in_ledger')
      tx_collateral(tx2, present: true)
      tx_collateral_outputs(tx2, present: false)
      tx_metadata(tx2, nil)
      tx_deposits(tx2, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx2, present: false)
      tx_mint_burn(tx2, mint: [], burn: [])
      tx_extra_signatures(tx2, present: true)
      tx_script_integrity(tx2, present: true)
      tx_validity_interval_default(tx2)
      tx_certificates(tx2, present: false)
    end

    it 'game' do
      contract_setup = 'game_1.json'
      scripts = ['game_2.json', 'game_3.json']

      run_contract(contract_setup, scripts)
    end

    it 'mint-burn' do
      vk = SHELLEY.keys.get_public_key(@wid, 'utxo_external', 0, { hash: true }).gsub('"', '')
      vk_hash = bech32_to_base16(vk)
      policy = read_mustached_file('mintBurn_policy', { vkHash: vk_hash })
      policy_id = get_policy_id(policy)
      def fingerprint
        if linux?
          'asset1q78ea9ds0rc3tfwu2damsjehjup2xuzddtg6xh'
        elsif mac?
          'asset1kjxaamf0p2p2z9g3k4xu0ne0g6h5j70st6z4pz'
        elsif win?
          'asset1arj5nz8zxjuxvut5wqt5q0xw7905hllugahvu7'
        end
      end

      def script_hash
        if linux?
          'c22560ac64be051102d6d1cfe5b9b82eb6af4f00dd3806e5cd82e837'
        elsif mac?
          '87c822cd8fb44f2e3bffc3eaf41c63c2301a0ac2325ee3db634bd435'
        elsif win?
          'c8a35944deea4a336faaeb88c35fee23ca88316eb698646e58a9298c'
        end
      end
      mint_script = 'mintBurn_1.json'
      burn_script = 'mintBurn_2.json'
      assets = [{ 'policy_script' => { 'script_info' => { 'language_version' => 'v1',
                                                          'script_hash' => script_hash },
                                       'script_type' => 'plutus' },
                  'policy_id' => policy_id,
                  'assets' => [{ 'fingerprint' => fingerprint,
                                 'quantity' => 1,
                                 'asset_name' => asset_name('mint-burn') }] }]

      payload_mint = get_templated_plutus_tx(mint_script, { vkHash: vk_hash,
                                                            policyId: policy_id,
                                                            policy: policy })

      payload_burn = get_templated_plutus_tx(burn_script, { vkHash: vk_hash,
                                                            policyId: policy_id,
                                                            policy: policy })
      mint = run_script(mint_script, payload_mint)
      burn = run_script(burn_script, payload_burn)

      # verify that decoded balanced tx is the same as signed tx (modulo witness_count)
      mint_balanced = mint[:tx_balanced].clone
      mint_signed = mint[:tx_signed].clone
      mint_balanced.delete('witness_count')
      mint_signed.delete('witness_count')
      expect(mint_balanced).to eq mint_signed
      burn_balanced = burn[:tx_balanced].clone
      burn_signed = burn[:tx_signed].clone
      burn_balanced.delete('witness_count')
      burn_signed.delete('witness_count')
      expect(burn_balanced).to eq burn_signed

      # verify witness count
      expect(mint[:tx_balanced]['witness_count']['verification_key']).to eq 0
      expect(mint[:tx_signed]['witness_count']['verification_key']).to be >= 1
      expect(burn[:tx_balanced]['witness_count']['verification_key']).to eq 0
      expect(burn[:tx_signed]['witness_count']['verification_key']).to be >= 1

      # verify decoded unbalanced transaction includes assets minted and burned
      expect(mint[:tx_unbalanced]['mint']['tokens']).to eq assets
      expect(mint[:tx_unbalanced]['burn']['tokens']).to eq []
      expect(burn[:tx_unbalanced]['mint']['tokens']).to eq []
      expect(burn[:tx_unbalanced]['burn']['tokens']).to eq assets

      # verify decoded balanced transaction includes assets minted and burned
      expect(mint[:tx_balanced]['mint']['tokens']).to eq assets
      expect(mint[:tx_balanced]['burn']['tokens']).to eq []
      expect(burn[:tx_balanced]['mint']['tokens']).to eq []
      expect(burn[:tx_balanced]['burn']['tokens']).to eq assets

      # examine tx history
      mint_tx = SHELLEY.transactions.get(@wid, mint[:tx_id])
      burn_tx = SHELLEY.transactions.get(@wid, burn[:tx_id])

      tx_inputs(mint_tx, present: true)
      tx_outputs(mint_tx, present: true)
      tx_direction(mint_tx, 'outgoing')
      tx_script_validity(mint_tx, 'valid')
      tx_status(mint_tx, 'in_ledger')
      tx_collateral(mint_tx, present: true)
      tx_collateral_outputs(mint_tx, present: false)
      tx_metadata(mint_tx, nil)
      tx_deposits(mint_tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(mint_tx, present: false)
      tx_mint_burn(mint_tx, mint: assets, burn: [])
      tx_extra_signatures(mint_tx, present: true)
      tx_script_integrity(mint_tx, present: true)
      tx_validity_interval_default(mint_tx)
      tx_certificates(mint_tx, present: false)

      tx_inputs(burn_tx, present: true)
      tx_outputs(burn_tx, present: true)
      tx_direction(burn_tx, 'outgoing')
      tx_script_validity(burn_tx, 'valid')
      tx_status(burn_tx, 'in_ledger')
      tx_collateral(burn_tx, present: true)
      tx_collateral_outputs(burn_tx, present: false)
      tx_metadata(burn_tx, nil)
      tx_deposits(burn_tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(burn_tx, present: false)
      tx_mint_burn(burn_tx, mint: [], burn: assets)
      tx_extra_signatures(burn_tx, present: true)
      tx_script_integrity(burn_tx, present: true)
      tx_validity_interval_default(burn_tx)
      tx_certificates(burn_tx, present: false)
    end

    it 'withdrawal' do
      ##
      # This test is withdrawing 0 rewards from stake account that has... 0 rewards.
      # Such tx is silly but allowed by the node.
      # Producing rewards on testnet is not practical due to long epoch length,
      # however there is full e2e test version of this script redeemding 42 A rewards on local cluster.
      #
      # NOTE:
      # The script cert had to be registered on-chain such that withdrawing is permitted.
      # It was done once, manually:
      #
      # 1. Create cert from Plutus script:
      #
      # $ cat fixtures/plutus/withdrawal_validator_cardano_cli.script
      # {"cborHex":"590853590850[...]cc0080080041","type":"PlutusScriptV1","description":""}
      # $ cardano-cli stake-address registration-certificate --stake-script-file fixtures/plutus/withdrawal_validator_cardano_cli.script --out-file stake.cert
      #
      # 2. Register cert on-chain
      #
      # $ cardano-cli query utxo --address $(cat payment.addr) --testnet-magic 2
      # $ cardano-cli transaction build  \
      # 	--babbage-era  \
      # 	--testnet-magic 2 \
      # 	--change-address "addr_test1qrfqc909vvxfq7903kaz09cuh5q2un8zw7j9ys4uh3k7j3qpgncz6fapajjvkyqka2sldfpk250nml40sf67am68wd2shl9fth" \
      # 	--tx-in "0c07395aed88bdddc6de0518d1462dd0ec7e52e1e3a53599f7cdb24dc80237f8#0"  \
      # 	--certificate-file stake.cert \
      # 	--protocol-params-file protocol.json  \
      # 	--out-file body.tx
      #
      # $ cardano-cli transaction sign \
      #    --tx-body-file body.tx \
      #    --testnet-magic 2 \
      #    --signing-key-file payment.skey \
      #    --out-file signed.tx
      #
      # $ cardano-cli transaction submit --tx-file signed.tx --testnet-magic 2
      validator = read_mustached_file('withdrawal_validator')
      validator_hash = get_policy_id(validator)
      withdrawal_script = 'withdrawal.json'
      payload = get_templated_plutus_tx(withdrawal_script, { script: validator,
                                                             scriptHash: validator_hash })

      init_src = get_shelley_balances(@wid)

      r = run_script(withdrawal_script, payload)

      # verify wallet balance decreases as expected by fee
      fee = r[:tx_balanced]['fee']['quantity']
      src_after = get_shelley_balances(@wid)
      expect(src_after['total']).to eq(init_src['total'] - fee)

      # examine tx history
      tx = SHELLEY.transactions.get(@wid, r[:tx_id])
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: true)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: true)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: true)
      tx_script_integrity(tx, present: true)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: false)
    end

    it 'currency' do
      currency_script = 'currency.json'
      currency_policy = 'currency_policy'

      # Perform coin selection to select input to be used in minting contract
      address = SHELLEY.addresses.list(@wid)[0]['id']
      payload_cs = [{ address: address,
                      amount: { quantity: 1_000_000_000, unit: 'lovelace' } }]
      coin_selection = SHELLEY.coin_selections.random(@wid, payload_cs)
      input = coin_selection['inputs'].select { |i| i['assets'] == [] }.first
      tx_id = input['id']
      tx_idx = input['index'].to_i
      amount = input['amount']['quantity'].to_i
      address = input['address']

      # encode input indexes for contract payload
      tx_idx_hex = tx_idx.to_cbor.cbor_to_hex # cbor as hex
      encoded_tx_idx = plutus_encode_idx(tx_idx) # special Plutus bit-wise encoding

      # feed payload for contract with data from coin selection
      policy = read_mustached_file(currency_policy, { transactionId: tx_id,
                                                      encodedTransactionIdx: encoded_tx_idx })
      policy_id = get_policy_id(policy)
      payload = get_templated_plutus_tx(currency_script, { policy: policy,
                                                           policyId: policy_id,
                                                           transactionId: tx_id,
                                                           transactionIdx: tx_idx,
                                                           transactionIdxHex: tx_idx_hex,
                                                           amount: amount,
                                                           address: address })

      # run contract
      r = run_script(currency_script, payload)

      # expected minted currency
      apfel = { 'policy_id' => policy_id,
                'asset_name' => asset_name('apfel'),
                'quantity' => 1000 }
      banana = { 'policy_id' => policy_id,
                 'asset_name' => asset_name('banana'),
                 'quantity' => 1 }

      # verify decoded transactions show that currency will be minted
      expect(r[:tx_unbalanced]['mint']['tokens'].to_s).to include policy_id
      expect(r[:tx_unbalanced]['mint']['tokens'].to_s).to include asset_name('apfel')
      expect(r[:tx_unbalanced]['mint']['tokens'].to_s).to include asset_name('banana')
      expect(r[:tx_balanced]['mint']['tokens'].to_s).to include policy_id
      expect(r[:tx_balanced]['mint']['tokens'].to_s).to include asset_name('apfel')
      expect(r[:tx_balanced]['mint']['tokens'].to_s).to include asset_name('banana')

      # make sure currency is minted as expected
      src_balance = get_shelley_balances(@wid)
      expect(src_balance['assets_total']).to include(apfel)
      expect(src_balance['assets_total']).to include(banana)

      # examine tx history
      tx = SHELLEY.transactions.get(@wid, r[:tx_id])
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: true)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: false)
      mint_tokens = tx['mint']['tokens'].to_s
      expect(mint_tokens).to include(asset_name('apfel'))
      expect(mint_tokens).to include(asset_name('banana'))
      expect(mint_tokens).to include(policy_id)
      expect(mint_tokens).to include('plutus')
      tx_mint_burn(tx, burn: [])
      tx_extra_signatures(tx, present: true)
      tx_script_integrity(tx, present: true)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: false)

      # send out minted currency to special address not to litter fixture wallet
      payment = [{ address: 'addr_test1qqkgrywfhejgd67twkzqmx84rsr3v374pzujd5rlm0e8exnlxjupjgrqwk5dk9tard6zfwwjq4lc89szs2w599js35tqmaykuj',
                   amount: { quantity: 0, unit: 'lovelace' },
                   assets: [apfel, banana] }]
      tx_constructed = SHELLEY.transactions.construct(@wid, payment)
      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      src_balance_after = get_shelley_balances(@wid)
      expect(src_balance_after['assets_total']).not_to include(apfel)
      expect(src_balance_after['assets_total']).not_to include(banana)
    end
  end

  describe 'E2E Construct -> Sign -> Submit' do
    it 'I can get min_utxo_value when contructing tx' do
      amt = 1
      tx_constructed = SHELLEY.transactions.construct(@wid, payment_payload(amt))
      expect(tx_constructed.code).to eq 403
      expect(tx_constructed['code']).to eq 'utxo_too_small'
      required_minimum = tx_constructed['info']['tx_output_lovelace_required_minimum']['quantity']

      tx_constructed = SHELLEY.transactions.construct(@wid, payment_payload(required_minimum))
      expect(tx_constructed).to be_correct_and_respond 202
    end

    it 'Single output transaction' do
      amt = MIN_UTXO_VALUE_PURE_ADA
      address = SHELLEY.addresses.list(@target_id)[0]['id']
      target_before = get_shelley_balances(@target_id)
      src_before = get_shelley_balances(@wid)

      tx_constructed = SHELLEY.transactions.construct(@wid, payment_payload(amt, address))
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
      expect(tx_decoded).to be_correct_and_respond 202
      # inputs are ours
      expect(tx_decoded['inputs'].to_s).to include 'address'
      expect(tx_decoded['inputs'].to_s).to include 'amount'
      expect(tx_decoded['outputs']).not_to eq []
      expect(tx_decoded['script_validity']).to eq 'valid'
      expect(tx_decoded['validity_interval']['invalid_before']).to eq({ 'quantity' => 0, 'unit' => 'slot' })
      expect(tx_decoded['validity_interval']['invalid_hereafter']['quantity']).to be > 0
      expect(tx_decoded['collateral']).to eq []
      expect(tx_decoded['collateral_outputs']).to eq []
      expect(tx_decoded['metadata']).to eq nil
      expect(tx_decoded['deposits_taken']).to eq []
      expect(tx_decoded['deposits_returned']).to eq []
      expect(tx_decoded['withdrawals']).to eq []
      expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
      expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
      expect(tx_decoded['certificates']).to eq []
      expect(tx_decoded['witness_count']['verification_key']).to eq 0

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202
      signed_decoded = SHELLEY.transactions.decode(@wid, tx_signed['transaction'])
      expect(signed_decoded['witness_count']['verification_key']).to be >= 1
      expect(expected_fee).to eq signed_decoded['fee']['quantity']

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      target_after = get_shelley_balances(@target_id)
      src_after = get_shelley_balances(@wid)

      # examine the tx in history
      # on src wallet
      tx = SHELLEY.transactions.get(@wid, tx_id)
      tx_amount(tx, amt + expected_fee)
      tx_fee(tx, expected_fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: false)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: false)

      # on target wallet
      txt = SHELLEY.transactions.get(@target_id, tx_id)
      tx_amount(txt, amt)
      tx_fee(tx, expected_fee)
      tx_inputs(txt, present: true)
      tx_outputs(txt, present: true)
      tx_direction(txt, 'incoming')
      tx_script_validity(txt, 'valid')
      tx_status(txt, 'in_ledger')
      tx_collateral(txt, present: false)
      tx_collateral_outputs(txt, present: false)
      tx_metadata(txt, nil)
      # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
      # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(txt, present: false)
      tx_mint_burn(txt, mint: [], burn: [])
      tx_extra_signatures(txt, present: false)
      tx_script_integrity(txt, present: false)
      tx_validity_interval_default(txt)
      tx_certificates(txt, present: false)

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amt, expected_fee)
    end

    it 'Multi output transaction' do
      amt = MIN_UTXO_VALUE_PURE_ADA
      address = SHELLEY.addresses.list(@target_id)[0]['id']
      target_before = get_shelley_balances(@target_id)
      src_before = get_shelley_balances(@wid)

      payment = [{ address: address,
                   amount: { quantity: amt,
                             unit: 'lovelace' } },
                 { address: address,
                   amount: { quantity: amt,
                             unit: 'lovelace' } }]
      tx_constructed = SHELLEY.transactions.construct(@wid, payment)
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
      expect(tx_decoded).to be_correct_and_respond 202
      # inputs are ours
      expect(tx_decoded['inputs'].to_s).to include 'address'
      expect(tx_decoded['inputs'].to_s).to include 'amount'
      expect(tx_decoded['outputs']).not_to eq []
      expect(tx_decoded['script_validity']).to eq 'valid'
      expect(tx_decoded['validity_interval']['invalid_before']).to eq({ 'quantity' => 0, 'unit' => 'slot' })
      expect(tx_decoded['validity_interval']['invalid_hereafter']['quantity']).to be > 0
      expect(tx_decoded['collateral']).to eq []
      expect(tx_decoded['collateral_outputs']).to eq []
      expect(tx_decoded['metadata']).to eq nil
      expect(tx_decoded['deposits_taken']).to eq []
      expect(tx_decoded['deposits_returned']).to eq []
      expect(tx_decoded['withdrawals']).to eq []
      expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
      expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
      expect(tx_decoded['certificates']).to eq []
      expect(tx_decoded['witness_count']['verification_key']).to eq 0

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202
      signed_decoded = SHELLEY.transactions.decode(@wid, tx_signed['transaction'])
      expect(signed_decoded['witness_count']['verification_key']).to be >= 1
      expect(expected_fee).to eq signed_decoded['fee']['quantity']

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      target_after = get_shelley_balances(@target_id)
      src_after = get_shelley_balances(@wid)

      # examine the tx in history
      # on src wallet
      tx = SHELLEY.transactions.get(@wid, tx_id)
      tx_amount(tx, (amt * 2) + expected_fee)
      tx_fee(tx, expected_fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: false)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: false)

      # on target wallet
      txt = SHELLEY.transactions.get(@target_id, tx_id)
      tx_amount(txt, amt * 2)
      tx_fee(tx, expected_fee)
      tx_inputs(txt, present: true)
      tx_outputs(txt, present: true)
      tx_direction(txt, 'incoming')
      tx_script_validity(txt, 'valid')
      tx_status(txt, 'in_ledger')
      tx_collateral(txt, present: false)
      tx_collateral_outputs(txt, present: false)
      tx_metadata(txt, nil)
      # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
      # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(txt, present: false)
      tx_mint_burn(txt, mint: [], burn: [])
      tx_extra_signatures(txt, present: false)
      tx_script_integrity(txt, present: false)
      tx_validity_interval_default(txt)
      tx_certificates(txt, present: false)

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         (amt * 2), expected_fee)
    end

    it 'Multi-assets transaction' do
      amt = 1
      amt_ada = 1_600_000
      address = SHELLEY.addresses.list(@target_id)[1]['id']
      target_before = get_shelley_balances(@target_id)
      src_before = get_shelley_balances(@wid)

      payment = [{ 'address' => address,
                   'amount' => { 'quantity' => amt_ada, 'unit' => 'lovelace' },
                   'assets' => [{ 'policy_id' => ASSETS[0]['policy_id'],
                                  'asset_name' => ASSETS[0]['asset_name'],
                                  'quantity' => amt },
                                { 'policy_id' => ASSETS[1]['policy_id'],
                                  'asset_name' => ASSETS[1]['asset_name'],
                                  'quantity' => amt }] }]

      tx_constructed = SHELLEY.transactions.construct(@wid, payment)
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
      expect(tx_decoded).to be_correct_and_respond 202
      # inputs are ours
      expect(tx_decoded['inputs'].to_s).to include 'address'
      expect(tx_decoded['inputs'].to_s).to include 'amount'
      expect(tx_decoded['outputs']).not_to eq []
      expect(tx_decoded['script_validity']).to eq 'valid'
      expect(tx_decoded['validity_interval']['invalid_before']).to eq({ 'quantity' => 0, 'unit' => 'slot' })
      expect(tx_decoded['validity_interval']['invalid_hereafter']['quantity']).to be > 0
      expect(tx_decoded['collateral']).to eq []
      expect(tx_decoded['collateral_outputs']).to eq []
      expect(tx_decoded['metadata']).to eq nil
      expect(tx_decoded['deposits_taken']).to eq []
      expect(tx_decoded['deposits_returned']).to eq []
      expect(tx_decoded['withdrawals']).to eq []
      expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
      expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
      expect(tx_decoded['certificates']).to eq []
      expect(tx_decoded['witness_count']['verification_key']).to eq 0

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202
      signed_decoded = SHELLEY.transactions.decode(@wid, tx_signed['transaction'])
      expect(signed_decoded['witness_count']['verification_key']).to be >= 1
      expect(expected_fee).to eq signed_decoded['fee']['quantity']

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      target_after = get_shelley_balances(@target_id)
      src_after = get_shelley_balances(@wid)

      # examine the tx in history
      # on src wallet
      tx = SHELLEY.transactions.get(@wid, tx_id)
      tx_amount(tx, amt_ada + expected_fee)
      tx_fee(tx, expected_fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: false)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: false)

      # on target wallet
      txt = SHELLEY.transactions.get(@target_id, tx_id)
      tx_amount(txt, amt_ada)
      tx_fee(tx, expected_fee)
      tx_inputs(txt, present: true)
      tx_outputs(txt, present: true)
      tx_direction(txt, 'incoming')
      tx_script_validity(txt, 'valid')
      tx_status(txt, 'in_ledger')
      tx_collateral(txt, present: false)
      tx_collateral_outputs(txt, present: false)
      tx_metadata(txt, nil)
      # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
      # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(txt, present: false)
      tx_mint_burn(txt, mint: [], burn: [])
      tx_extra_signatures(txt, present: false)
      tx_script_integrity(txt, present: false)
      tx_validity_interval_default(txt)
      tx_certificates(txt, present: false)

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amt_ada, expected_fee)

      verify_asset_balance(src_after, src_before,
                           target_after, target_before,
                           amt)

      # Target wallet lists my associated assets
      assets = SHELLEY.assets.get(@target_id)
      expect(assets).to be_correct_and_respond 200
      expect(assets.to_s).to include ASSETS[0]['policy_id']
      expect(assets.to_s).to include ASSETS[0]['asset_name']
      expect(assets.to_s).to include ASSETS[1]['policy_id']
      expect(assets.to_s).to include ASSETS[1]['asset_name']
    end

    it 'Only withdrawal' do
      balance = get_shelley_balances(@wid)
      tx_constructed = SHELLEY.transactions.construct(@wid,
                                                      nil, # payments
                                                      'self') # withdrawal
      expect(tx_constructed).to be_correct_and_respond 202
      # withdrawal = tx_constructed['coin_selection']['withdrawals'].map { |x| x['amount']['quantity'] }.first
      # expect(withdrawal).to eq 0
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
      expect(tx_decoded).to be_correct_and_respond 202
      # inputs are ours
      expect(tx_decoded['inputs'].to_s).to include 'address'
      expect(tx_decoded['inputs'].to_s).to include 'amount'
      expect(tx_decoded['outputs']).not_to eq []
      expect(tx_decoded['script_validity']).to eq 'valid'
      expect(tx_decoded['validity_interval']['invalid_before']).to eq({ 'quantity' => 0, 'unit' => 'slot' })
      expect(tx_decoded['validity_interval']['invalid_hereafter']['quantity']).to be > 0
      expect(tx_decoded['collateral']).to eq []
      expect(tx_decoded['collateral_outputs']).to eq []
      expect(tx_decoded['metadata']).to eq nil
      expect(tx_decoded['deposits_taken']).to eq []
      expect(tx_decoded['deposits_returned']).to eq []
      expect(tx_decoded['withdrawals']).to eq []
      expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
      expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
      expect(tx_decoded['certificates']).to eq []
      expect(tx_decoded['witness_count']['verification_key']).to eq 0

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202
      signed_decoded = SHELLEY.transactions.decode(@wid, tx_signed['transaction'])
      expect(signed_decoded['witness_count']['verification_key']).to be >= 1
      expect(expected_fee).to eq signed_decoded['fee']['quantity']

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      # examine the tx in history
      # on src wallet
      tx = SHELLEY.transactions.get(@wid, tx_id)
      tx_amount(tx, expected_fee)
      tx_fee(tx, expected_fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: false) # tx has no withdrawals
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: false)

      # verify balance is as expected
      new_balance = get_shelley_balances(@wid)
      expect(new_balance['available']).to eq(balance['available'] - expected_fee)
      expect(new_balance['total']).to eq(balance['total'] - expected_fee)
    end

    it 'Only metadata' do
      metadata = METADATA
      balance = get_shelley_balances(@wid)
      tx_constructed = SHELLEY.transactions.construct(@wid,
                                                      nil, # payments
                                                      nil, # withdrawal
                                                      metadata)
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
      expect(tx_decoded).to be_correct_and_respond 202

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      # inputs are ours
      expect(tx_decoded['inputs'].to_s).to include 'address'
      expect(tx_decoded['inputs'].to_s).to include 'amount'
      expect(tx_decoded['outputs']).not_to eq []
      expect(tx_decoded['script_validity']).to eq 'valid'
      expect(tx_decoded['validity_interval']['invalid_before']).to eq({ 'quantity' => 0, 'unit' => 'slot' })
      expect(tx_decoded['validity_interval']['invalid_hereafter']['quantity']).to be > 0
      expect(tx_decoded['collateral']).to eq []
      expect(tx_decoded['collateral_outputs']).to eq []
      expect(tx_decoded['metadata']).to eq metadata
      expect(tx_decoded['deposits_taken']).to eq []
      expect(tx_decoded['deposits_returned']).to eq []
      expect(tx_decoded['withdrawals']).to eq []
      expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
      expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
      expect(tx_decoded['certificates']).to eq []
      expect(tx_decoded['witness_count']['verification_key']).to eq 0

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202
      signed_decoded = SHELLEY.transactions.decode(@wid, tx_signed['transaction'])
      expect(signed_decoded['witness_count']['verification_key']).to be >= 1
      expect(expected_fee).to eq signed_decoded['fee']['quantity']

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      # examine the tx in history
      # on src wallet
      tx = SHELLEY.transactions.get(@wid, tx_id)
      tx_amount(tx, expected_fee)
      tx_fee(tx, expected_fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, metadata)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: false)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: false)

      # verify balance is as expected
      new_balance = get_shelley_balances(@wid)
      expect(new_balance['available']).to eq(balance['available'] - expected_fee)
      expect(new_balance['total']).to eq(balance['total'] - expected_fee)
    end

    it 'Validity intervals' do
      amt = MIN_UTXO_VALUE_PURE_ADA
      address = SHELLEY.addresses.list(@target_id)[0]['id']
      target_before = get_shelley_balances(@target_id)
      src_before = get_shelley_balances(@wid)
      inv_before = 500
      inv_hereafter = 5_000_000_000
      validity_interval = { 'invalid_before' => { 'quantity' => inv_before, 'unit' => 'slot' },
                            'invalid_hereafter' => { 'quantity' => inv_hereafter, 'unit' => 'slot' } }

      tx_constructed = SHELLEY.transactions.construct(@wid,
                                                      payment_payload(amt, address),
                                                      nil, # withdrawal
                                                      nil, # metadata
                                                      nil, # delegations
                                                      nil, # mint_burn
                                                      validity_interval)
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
      expect(tx_decoded).to be_correct_and_respond 202
      # inputs are ours
      expect(tx_decoded['inputs'].to_s).to include 'address'
      expect(tx_decoded['inputs'].to_s).to include 'amount'
      expect(tx_decoded['outputs']).not_to eq []
      expect(tx_decoded['script_validity']).to eq 'valid'
      expect(tx_decoded['validity_interval']['invalid_before']).to eq validity_interval['invalid_before']
      expect(tx_decoded['validity_interval']['invalid_hereafter']).to eq validity_interval['invalid_hereafter']
      expect(tx_decoded['collateral']).to eq []
      expect(tx_decoded['collateral_outputs']).to eq []
      expect(tx_decoded['metadata']).to eq nil
      expect(tx_decoded['deposits_taken']).to eq []
      expect(tx_decoded['deposits_returned']).to eq []
      expect(tx_decoded['withdrawals']).to eq []
      expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
      expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
      expect(tx_decoded['certificates']).to eq []
      expect(tx_decoded['witness_count']['verification_key']).to eq 0

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202
      signed_decoded = SHELLEY.transactions.decode(@wid, tx_signed['transaction'])
      expect(signed_decoded['witness_count']['verification_key']).to be >= 1
      expect(expected_fee).to eq signed_decoded['fee']['quantity']

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      target_after = get_shelley_balances(@target_id)
      src_after = get_shelley_balances(@wid)

      # examine the tx in history
      # on src wallet
      tx = SHELLEY.transactions.get(@wid, tx_id)
      tx_amount(tx, amt + expected_fee)
      tx_fee(tx, expected_fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: false)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval(tx, invalid_before: inv_before, invalid_hereafter: inv_hereafter)
      tx_certificates(tx, present: false)

      # on target wallet
      txt = SHELLEY.transactions.get(@target_id, tx_id)
      tx_amount(txt, amt)
      tx_fee(tx, expected_fee)
      tx_inputs(txt, present: true)
      tx_outputs(txt, present: true)
      tx_direction(txt, 'incoming')
      tx_script_validity(txt, 'valid')
      tx_status(txt, 'in_ledger')
      tx_collateral(txt, present: false)
      tx_collateral_outputs(txt, present: false)
      tx_metadata(txt, nil)
      # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
      # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(txt, present: false)
      tx_mint_burn(txt, mint: [], burn: [])
      tx_extra_signatures(txt, present: false)
      tx_script_integrity(txt, present: false)
      tx_validity_interval(txt, invalid_before: inv_before, invalid_hereafter: inv_hereafter)
      tx_certificates(txt, present: false)

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amt, expected_fee)
    end

    it 'Delegation (join and quit)' do
      skip 'ADP-3243'
      balance = get_shelley_balances(@target_id)
      expected_deposit = CARDANO_CLI.protocol_params['stakeAddressDeposit']
      puts "Expected deposit #{expected_deposit}"
      # Check wallet stake keys before joing stake pool
      stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
      expect(stake_keys).to be_correct_and_respond 200
      expect(stake_keys['foreign'].size).to eq 0
      expect(stake_keys['ours'].size).to eq 1
      expect(stake_keys['ours'].first['stake']['quantity']).to eq balance['total']
      expect(stake_keys['none']['stake']['quantity']).to eq 0
      expect(stake_keys['ours'].first['delegation']['active']['status']).to eq 'not_delegating'

      # Pick up pool id to join
      pools = SHELLEY.stake_pools
      pool_id = pools.list({ stake: 1000 }).sample['id']

      # Join pool
      delegation = [{
        'join' => {
          'pool' => pool_id,
          'stake_key_index' => '0H'
        }
      }]
      tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@target_id,
                                                                      nil, # payments
                                                                      nil, # withdrawal
                                                                      nil, # metadata
                                                                      delegation)
      # Check fee and deposit on joining
      decoded_tx = SHELLEY.transactions.decode(@target_id, tx_constructed['transaction'])
      deposit_taken = tx_constructed['coin_selection']['deposits_taken'].first['quantity']
      decoded_deposit_taken = decoded_tx['deposits_taken'].first['quantity']
      expect(deposit_taken).to eq decoded_deposit_taken
      expect(deposit_taken).to eq expected_deposit
      expect(decoded_tx['deposits_returned']).to eq []

      expected_fee = tx_constructed['fee']['quantity']
      decoded_fee = decoded_tx['fee']['quantity']
      expect(decoded_fee).to eq expected_fee

      # witness count
      expect(decoded_tx['witness_count']['verification_key']).to eq 0
      signed_tx = SHELLEY.transactions.decode(@target_id, tx_signed['transaction'])
      expect(signed_tx['witness_count']['verification_key']).to be >= 1

      # Certificates
      expect(decoded_tx['certificates']).to include(have_key('certificate_type')).twice
      expect(decoded_tx['certificates']).to include(have_value('register_reward_account')).once
      expect(decoded_tx['certificates']).to include(have_value('join_pool')).once
      expect(decoded_tx['certificates']).to include(have_key('reward_account_path')).twice
      expect(decoded_tx['certificates']).to include(have_value(%w[1852H 1815H 0H 2 0])).twice
      expect(decoded_tx['certificates']).to include(have_key('pool')).once
      expect(decoded_tx['certificates']).to include(have_value(pool_id)).once

      tx_id = tx_submitted['id']
      wait_for_tx_in_ledger(@target_id, tx_id)

      # Check fee and balance and deposit after joining
      tx = SHELLEY.transactions.get(@target_id, tx_id)
      tx_amount(tx, expected_deposit + expected_fee)
      tx_fee(tx, expected_fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: deposit_taken, deposit_returned: 0)
      tx_withdrawals(tx, present: false)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: true, certificates: decoded_tx['certificates'])
      expect(tx['certificates'].to_s).to include 'register_reward_account'
      expect(tx['certificates'].to_s).to include 'join_pool'
      expect(tx['certificates'].to_s).to include pool_id

      join_balance = get_shelley_balances(@target_id)
      expected_join_balance = balance['total'] - deposit_taken - expected_fee
      expect(join_balance['total']).to eq expected_join_balance

      # Check wallet stake keys after joing stake pool
      stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
      expect(stake_keys).to be_correct_and_respond 200
      expect(stake_keys['foreign'].size).to eq 0
      expect(stake_keys['ours'].size).to eq 1
      expect(stake_keys['ours'].first['stake']['quantity']).to eq expected_join_balance
      expect(stake_keys['none']['stake']['quantity']).to eq 0
      expect(stake_keys['ours'].first['delegation']['active']['status']).to eq 'not_delegating'
      expect(stake_keys['ours'].first['delegation']['next'].last['status']).to eq 'delegating'

      # Quit pool
      quit_pool = [{ 'quit' => { 'stake_key_index' => '0H' } }]
      tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@target_id,
                                                                      nil, # payments
                                                                      nil, # withdrawal
                                                                      nil, # metadata
                                                                      quit_pool)

      # Check fee and deposit on quitting
      decoded_tx = SHELLEY.transactions.decode(@target_id, tx_constructed['transaction'])
      expect(decoded_tx).to be_correct_and_respond 202

      # witness count
      expect(decoded_tx['witness_count']['verification_key']).to eq 0
      signed_tx = SHELLEY.transactions.decode(@target_id, tx_signed['transaction'])
      expect(signed_tx['witness_count']['verification_key']).to be >= 1

      # Certificates
      expect(decoded_tx['certificates']).to include(have_key('certificate_type')).once
      expect(decoded_tx['certificates']).to include(have_value('quit_pool')).once
      expect(decoded_tx['certificates']).to include(have_key('reward_account_path')).once
      expect(decoded_tx['certificates']).to include(have_value(%w[1852H 1815H 0H 2 0])).once
      expect(decoded_tx['certificates']).not_to include(have_value('register_reward_account'))
      expect(decoded_tx['certificates']).not_to include(have_key('pool')).once
      expect(decoded_tx['certificates']).not_to include(have_value(pool_id)).once

      expect(tx_constructed['coin_selection']['deposits_taken']).to eq []
      expect(decoded_tx['deposits_taken']).to eq []
      deposit_returned = tx_constructed['coin_selection']['deposits_returned'].first['quantity']
      decoded_deposit_returned = decoded_tx['deposits_returned'].first['quantity']
      expect(deposit_returned).to eq decoded_deposit_returned
      expect(deposit_returned).to eq expected_deposit

      expected_fee = tx_constructed['fee']['quantity']
      decoded_fee = decoded_tx['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_id = tx_submitted['id']
      wait_for_tx_in_ledger(@target_id, tx_id)

      # Check fee and balance and deposit after quitting
      quit_balance = get_shelley_balances(@target_id)
      tx = SHELLEY.transactions.get(@target_id, tx_id)
      expect(tx['amount']['quantity']).to be > 0
      expect(tx['amount']['quantity']).to be < deposit_returned
      tx_fee(tx, expected_fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'incoming')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
      # tx_deposits(tx, deposit_taken: 0, deposit_returned: deposit_returned)
      tx_withdrawals(tx, present: false)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: true, certificates: decoded_tx['certificates'])
      expect(tx['certificates'].to_s).to include 'quit_pool'

      expected_quit_balance = join_balance['total'] + deposit_returned - expected_fee
      expect(quit_balance['total']).to eq expected_quit_balance

      # Check wallet stake keys after quitting
      stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
      expect(stake_keys).to be_correct_and_respond 200
      expect(stake_keys['foreign'].size).to eq 0
      expect(stake_keys['ours'].size).to eq 1
      # deposit is back on quitting so stake is higher than before
      expect(stake_keys['ours'].first['stake']['quantity']).to eq expected_quit_balance
      expect(stake_keys['none']['stake']['quantity']).to eq 0
      expect(stake_keys['ours'].first['delegation']['active']['status']).to eq 'not_delegating'
      expect(stake_keys['ours'].first['delegation']['next'].last['status']).to eq 'not_delegating'
    end

    describe 'Minting and Burning' do
      ##
      # Tx1: Mints 3 x 1000 assets, each guarded by different policy script
      # Tx2: Burns 3 x 500 of each and verifies 500 of each remain on wallet
      # Tx3: Burns remaining 3 x 500 and verifies they're no longer on balance
      it 'Can mint and then burn' do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script1 = 'cosigner#0'
        policy_script2 = { 'all' => ['cosigner#0'] }
        policy_script3 = { 'any' => ['cosigner#0'] }
        # Get policy_ids:
        policy_id1 = SHELLEY.keys.create_policy_id(@wid, policy_script1)['policy_id']
        policy_id2 = SHELLEY.keys.create_policy_id(@wid, policy_script2)['policy_id']
        policy_id3 = SHELLEY.keys.create_policy_id(@wid, policy_script3)['policy_id']
        # Get policy key hash
        policy_vkh = SHELLEY.keys.get_policy_key(@wid, { hash: true }).gsub('"', '')

        # Minting:
        mint = [mint(asset_name('Token1'), 1000, policy_script1, address),
                mint(asset_name('Token2'), 1000, policy_script2),
                mint('', 1000, policy_script3)]
        create_policy_key_if_not_exists(@wid)
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        nil, # payments
                                                                        nil, # withdrawal
                                                                        nil, # metadata
                                                                        nil, # delegations
                                                                        mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # witness count
        expect(tx_decoded['witness_count']['verification_key']).to eq 0
        signed_tx = SHELLEY.transactions.decode(@wid, tx_signed['transaction'])
        expect(signed_tx['witness_count']['verification_key']).to be >= 1

        # Mint
        expect(tx_decoded['mint']['tokens']).to include(have_key('assets')).exactly(3).times
        expect(tx_decoded['mint']['tokens'].to_s).to include(policy_vkh).exactly(3).times
        expect(tx_decoded['mint']['tokens'].to_s).to include('1000').exactly(3).times
        expect(tx_decoded['mint']['tokens'].to_s).to include(policy_id1).once
        expect(tx_decoded['mint']['tokens'].to_s).to include(policy_id2).once
        expect(tx_decoded['mint']['tokens'].to_s).to include(policy_id3).once
        expect(tx_decoded['mint']['tokens'].to_s).to include(asset_name('Token1')).once
        expect(tx_decoded['mint']['tokens'].to_s).to include(asset_name('Token2')).once

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)

        # examine the tx in history
        # on src wallet
        tx = SHELLEY.transactions.get(@wid, tx_id)
        tx_amount(tx, expected_fee)
        tx_fee(tx, expected_fee)
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'outgoing')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, nil)
        tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(tx, present: false)
        expect(tx['mint']).to eq tx_decoded['mint']
        tx_mint_burn(tx, mint: tx_decoded['mint']['tokens'], burn: [])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: false)

        # verify ADA balance is correct (fee is deducted)
        src_after_minting = get_shelley_balances(@wid)
        expect(src_after_minting['available']).to eq(src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq(src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.to_set { |z| { z => 1000 } })

        # Burn half:
        burn = [burn(asset_name('Token1'), 500, policy_script1),
                burn(asset_name('Token2'), 500, policy_script2),
                burn('', 500, policy_script3)]
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        nil, # payments
                                                                        nil, # withdrawal
                                                                        nil, # metadata
                                                                        nil, # delegations
                                                                        burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # witness count
        expect(tx_decoded['witness_count']['verification_key']).to eq 0
        signed_tx = SHELLEY.transactions.decode(@target_id, tx_signed['transaction'])
        expect(signed_tx['witness_count']['verification_key']).to be >= 1

        # Burn
        expect(tx_decoded['burn']['tokens']).to include(have_key('assets')).exactly(3).times
        expect(tx_decoded['burn']['tokens'].to_s).to include(policy_vkh).exactly(3).times
        expect(tx_decoded['burn']['tokens'].to_s).to include('500').exactly(3).times
        expect(tx_decoded['burn']['tokens'].to_s).to include(policy_id1).once
        expect(tx_decoded['burn']['tokens'].to_s).to include(policy_id2).once
        expect(tx_decoded['burn']['tokens'].to_s).to include(policy_id3).once
        expect(tx_decoded['burn']['tokens'].to_s).to include(asset_name('Token1')).once
        expect(tx_decoded['burn']['tokens'].to_s).to include(asset_name('Token2')).once

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        # examine the burn tx in history
        # on src wallet
        tx = SHELLEY.transactions.get(@wid, tx_id)
        tx_amount(tx, expected_fee)
        tx_fee(tx, expected_fee)
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'outgoing')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, nil)
        tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(tx, present: false)
        expect(tx['burn']).to eq tx_decoded['burn']
        tx_mint_burn(tx, mint: [], burn: tx_decoded['burn']['tokens'])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: false)

        # verify ADA balance is correct (fee is deducted)
        src_after_burning = get_shelley_balances(@wid)
        expect(src_after_burning['available']).to eq(src_after_minting['available'] - expected_fee)
        expect(src_after_burning['total']).to eq(src_after_minting['total'] - expected_fee)

        # verify half of assets have ben burned
        assets = assets_balance(src_after_burning['assets_total'],
                                { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.to_set { |z| { z => 500 } })

        # Burn all the rest:
        burn = [burn(asset_name('Token1'), 500, policy_script1),
                burn(asset_name('Token2'), 500, policy_script2),
                burn(nil, 500, policy_script3)]
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # Burn again
        expect(tx_decoded['burn']['tokens']).to include(have_key('assets')).exactly(3).times
        expect(tx_decoded['burn']['tokens'].to_s).to include(policy_vkh).exactly(3).times
        expect(tx_decoded['burn']['tokens'].to_s).to include('500').exactly(3).times
        expect(tx_decoded['burn']['tokens'].to_s).to include(policy_id1).once
        expect(tx_decoded['burn']['tokens'].to_s).to include(policy_id2).once
        expect(tx_decoded['burn']['tokens'].to_s).to include(policy_id3).once
        expect(tx_decoded['burn']['tokens'].to_s).to include(asset_name('Token1')).once
        expect(tx_decoded['burn']['tokens'].to_s).to include(asset_name('Token2')).once

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning_all = get_shelley_balances(@wid)
        # examine the burn again tx in history
        # on src wallet
        tx = SHELLEY.transactions.get(@wid, tx_id)
        tx_amount(tx, expected_fee)
        tx_fee(tx, expected_fee)
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'outgoing')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, nil)
        tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(tx, present: false)
        expect(tx['burn']).to eq tx_decoded['burn']
        tx_mint_burn(tx, mint: [], burn: tx_decoded['burn']['tokens'])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: false)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning_all['available']).to eq(src_after_burning['available'] - expected_fee)
        expect(src_after_burning_all['total']).to eq(src_after_burning['total'] - expected_fee)

        # verify all assets have been burned and no longer on wallet's balance
        assets = assets_balance(src_after_burning_all['assets_total'],
                                { assets_to_check: assets_to_check })
        expect(assets).to eq({}.to_set)
      end

      ##
      # Tx1: Mints 3 x 1 assets with metadata
      # Tx2: Burns 3 x 1 assets and also assign metadata to tx
      it 'Can mint and burn with metadata' do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script1 = 'cosigner#0'
        policy_script2 = 'cosigner#0'
        policy_script3 = { 'any' => ['cosigner#0'] }
        metadata = METADATA
        assets_quantity = 1

        # Minting:
        mint = [mint(asset_name('TokenMetadata1'), assets_quantity, policy_script1, address),
                mint(asset_name('TokenMetadata2'), assets_quantity, policy_script2),
                mint(asset_name('TokenMetadata3'), assets_quantity, policy_script3)]
        create_policy_key_if_not_exists(@wid)
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                metadata,
                                                                nil, # delegations
                                                                mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting = get_shelley_balances(@wid)

        # verify tx has metadata
        tx = SHELLEY.transactions.get(@wid, tx_id)
        expect(tx['metadata']).to eq metadata

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting['available']).to eq(src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq(src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.to_set { |z| { z => assets_quantity } })

        # Burn all:
        burn = [burn(asset_name('TokenMetadata1'), assets_quantity, policy_script1),
                burn(asset_name('TokenMetadata2'), assets_quantity, policy_script2),
                burn(asset_name('TokenMetadata3'), assets_quantity, policy_script3)]
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                metadata,
                                                                nil, # delegations
                                                                burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning = get_shelley_balances(@wid)

        # verify tx has metadata
        tx = SHELLEY.transactions.get(@wid, tx_id)
        expect(tx['metadata']).to eq metadata

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning['available']).to eq(src_after_minting['available'] - expected_fee)
        expect(src_after_burning['total']).to eq(src_after_minting['total'] - expected_fee)

        # verify all assets have been burned and no longer on wallet's balance
        assets = assets_balance(src_after_burning['assets_total'],
                                { assets_to_check: assets_to_check })
        expect(assets).to eq({}.to_set)
      end

      ##
      # Mint NFT with CIP-25 metadata
      it 'Can mint NFT attaching CIP-25 metadata' do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script = 'cosigner#0'
        assets_quantity = 1
        nft_name = 'MyAmazingNFT'
        nft_name_hex = asset_name(nft_name)
        mint = [mint(nft_name_hex, assets_quantity, policy_script, address)]

        create_policy_key_if_not_exists(@wid)

        # Get policy_id:
        policy_id = SHELLEY.keys.create_policy_id(@wid, policy_script)['policy_id']

        # Build CIP-25 metadata
        cip25_metadata = { '721' => {
          policy_id.to_s => {
            nft_name.to_s => {
              'name' => "NFT FTW: #{nft_name}",
              'image' => 'ipfs://XXXXYYYYZZZZ'
            }
          }
        } }

        # Minting:
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                cip25_metadata,
                                                                nil, # delegations
                                                                mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting = get_shelley_balances(@wid)

        # verify tx has metadata
        tx = SHELLEY.transactions.get(@wid, tx_id, 'simple-metadata' => true)
        expect(tx['metadata']).to eq cip25_metadata

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting['available']).to eq(src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq(src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.to_set { |z| { z => assets_quantity } })

        # Burn:
        burn = [burn(nft_name_hex, assets_quantity, policy_script)]

        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                burn)
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning['available']).to eq(src_after_minting['available'] - expected_fee)
        expect(src_after_burning['total']).to eq(src_after_minting['total'] - expected_fee)

        # verify all assets have been burned and no longer on wallet's balance
        assets = assets_balance(src_after_burning['assets_total'],
                                { assets_to_check: assets_to_check })
        expect(assets).to eq({}.to_set)
      end

      ##
      # Tx1: Mints 2 x 500 assets, each guarded by different policy script => A1 = 500, A2 = 500
      # Tx2: Mints 500 more of A1 and burns 500 of A2 => A1 = 1000, A2 = 0
      # Tx3: Burns remaining 1000 of A1 => A1 = 0, A2 = 0
      it 'Can mint and burn in the same tx' do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script1 = { 'some' => { 'at_least' => 1, 'from' => ['cosigner#0'] } }
        policy_script2 = { 'any' => ['cosigner#0'] }

        # Minting:
        mint = [mint(asset_name('Asset1'), 500, policy_script1, address),
                mint(asset_name('Asset2'), 500, policy_script2)]

        create_policy_key_if_not_exists(@wid)
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting['available']).to eq(src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq(src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.to_set { |z| { z => 500 } })

        # Minting and burning:
        mint_burn = [mint(asset_name('Asset1'), 500, policy_script1),
                     burn(asset_name('Asset2'), 500, policy_script2)]

        # p JSON.parse(mint_burn.to_json)
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                mint_burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting_burning['available']).to eq(src_after_minting['available'] - expected_fee)
        expect(src_after_minting_burning['total']).to eq(src_after_minting['total'] - expected_fee)

        # verify Asset1 has been minted and Asset2 burned
        assets_minted_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets_burned_to_check = get_assets_from_decode(tx_decoded['burn'])
        assets_minted = assets_balance(src_after_minting_burning['assets_total'],
                                       { assets_to_check: assets_minted_to_check })
        assets_burned = assets_balance(src_after_minting_burning['assets_total'],
                                       { assets_to_check: assets_burned_to_check })

        expect(assets_minted).to eq(assets_minted_to_check.to_set { |z| { z => 1000 } })
        expect(assets_burned).to eq({}.to_set)

        # Burn all the rest:
        burn = [burn(asset_name('Asset1'), 1000, policy_script1)]
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                burn)

        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning['available']).to eq(src_after_minting_burning['available'] - expected_fee)
        expect(src_after_burning['total']).to eq(src_after_minting_burning['total'] - expected_fee)

        # verify Asset1 has been burned
        assets_burned_to_check = get_assets_from_decode(tx_decoded['burn'])
        assets_burned = assets_balance(src_after_burning['assets_total'],
                                       { assets_to_check: assets_burned_to_check })
        expect(assets_burned).to eq({}.to_set)
      end

      ##
      # Tx1: Mints 10 asset => has 10
      # Tx2: Burns 10 and mints 1 of the same asset => has 1
      # Tx3: Burns 1 remaining assets => has 0
      it 'Can mint and burn the same asset in single tx' do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script = 'cosigner#0'
        assets_name = asset_name('MintBurnX')

        # Minting 10 MintBurn:
        mint = [mint(assets_name, 10, policy_script, address)]

        create_policy_key_if_not_exists(@wid)
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting['available']).to eq(src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq(src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.to_set { |z| { z => 10 } })

        # Burning 10 MintBurn and minting 1 MintBurn:
        mint_burn = [burn(assets_name, 10, policy_script),
                     mint(assets_name, 1, policy_script, address)]

        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                mint_burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting_burning['available']).to eq(src_after_minting['available'] - expected_fee)
        expect(src_after_minting_burning['total']).to eq(src_after_minting['total'] - expected_fee)

        # verify MintBurn has 1 (because 10 was burned and 1 additional minted)
        assets_minted_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets_minted = assets_balance(src_after_minting_burning['assets_total'],
                                       { assets_to_check: assets_minted_to_check })

        expect(assets_minted).to eq(assets_minted_to_check.to_set { |z| { z => 1 } })

        # Burn all the rest:
        burn = [burn(assets_name, 1, policy_script)]
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                burn)

        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning['available']).to eq(src_after_minting_burning['available'] - expected_fee)
        expect(src_after_burning['total']).to eq(src_after_minting_burning['total'] - expected_fee)

        # verify all is burned
        assets_burned_to_check = get_assets_from_decode(tx_decoded['burn'])
        assets_burned = assets_balance(src_after_burning['assets_total'],
                                       { assets_to_check: assets_burned_to_check })
        expect(assets_burned).to eq({}.to_set)
      end

      ##
      # Tx1: Fail to burn asset that's not on the wallet
      it "Cannot burn if I don't have it" do
        policy_script = 'cosigner#0'
        burn = [burn(asset_name('AmazingNFTIdontHave'), 1, policy_script)]
        create_policy_key_if_not_exists(@wid)
        tx_constructed = SHELLEY.transactions.construct(@wid,
                                                        nil, # payments
                                                        nil, # withdrawal
                                                        nil, # metadata
                                                        nil, # delegations
                                                        burn)
        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code'].to_s).to eq 'not_enough_money'
        expect(tx_constructed['message'].to_s).to include "assetName: #{asset_name('AmazingNFTIdontHave')}"
        expect(tx_constructed['message'].to_s).to include 'quantity: 1'
      end

      ##
      # Tx1: Mints 1000 assets
      # Tx2: Fails to burn 1000 assets using different policy_script
      # Tx2: Fails to burn 1022 assets using correct policy_script
      # Tx3: Burns remaining 1000 assets
      it 'Cannot burn with wrong policy_script or more than I have' do
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script = 'cosigner#0'

        # Mint it:
        mint = [mint(asset_name('MintIt'), 1000, policy_script, address)]
        create_policy_key_if_not_exists(@wid)
        _, _, tx_submitted = construct_sign_submit(@wid,
                                                   nil, # payments
                                                   nil, # withdrawal
                                                   nil, # metadata
                                                   nil, # delegations
                                                   mint)

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)

        # Try to burn:
        #  - with different policy_script
        burn1 = [burn(asset_name('MintIt'), 1000, { 'all' => ['cosigner#0'] })]
        tx_constructed = SHELLEY.transactions.construct(@wid,
                                                        nil, # payments
                                                        nil, # withdrawal
                                                        nil, # metadata
                                                        nil, # delegations
                                                        burn1)

        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code'].to_s).to eq 'not_enough_money'
        expect(tx_constructed['message'].to_s).to include "assetName: #{asset_name('MintIt')}"
        expect(tx_constructed['message'].to_s).to include 'quantity: 1000'

        #  - correct policy_script but too much
        burn2 = [burn(asset_name('MintIt'), 1022, policy_script)]
        tx_constructed = SHELLEY.transactions.construct(@wid,
                                                        nil, # payments
                                                        nil, # withdrawal
                                                        nil, # metadata
                                                        nil, # delegations
                                                        burn2)

        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code'].to_s).to eq 'not_enough_money'
        expect(tx_constructed['message'].to_s).to include "assetName: #{asset_name('MintIt')}"
        expect(tx_constructed['message'].to_s).to include 'quantity: 22'

        # Burn it:
        burn3 = [burn(asset_name('MintIt'), 1000, policy_script)]
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                burn3)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning = get_shelley_balances(@wid)

        # verify MintIt has been burned
        assets_burned_to_check = get_assets_from_decode(tx_decoded['burn'])
        assets_burned = assets_balance(src_after_burning['assets_total'],
                                       { assets_to_check: assets_burned_to_check })
        expect(assets_burned).to eq({}.to_set)
      end

      ##
      # Tx1: Mints 1500 assets to different wallet
      # Tx2: Fails to burn 1500 assets on different wallet
      # Tx3: Sends assets back to src wallet
      # Tx4: Burns them
      it "Mint to foreign wallet / Cannot burn if I don't have keys" do
        address = SHELLEY.addresses.list(@target_id).first['id']
        policy_script = 'cosigner#0'
        assets_quantity = 1500
        assets_name = asset_name('ToForeignWallet')

        # Mint it:
        mint = [mint(assets_name, assets_quantity, policy_script, address)]
        create_policy_key_if_not_exists(@wid)
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                mint)

        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        target_after_minting = get_shelley_balances(@target_id)

        # verify assets have been minted and on target wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(target_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.to_set { |z| { z => assets_quantity } })

        # Try to burn on target wallet and fail:
        create_policy_key_if_not_exists(@target_id)
        burn = [burn(assets_name, assets_quantity, policy_script)]
        tx_constructed = SHELLEY.transactions.construct(@target_id,
                                                        nil, # payments
                                                        nil, # withdrawal
                                                        nil, # metadata
                                                        nil, # delegations
                                                        burn)
        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code'].to_s).to eq 'not_enough_money'
        expect(tx_constructed['message'].to_s).to include "assetName: #{assets_name}"
        expect(tx_constructed['message'].to_s).to include "quantity: #{assets_quantity}"

        # Send them back to src wallet:
        src_address = SHELLEY.addresses.list(@wid).first['id']
        policy_id = get_policy_id_from_decode(tx_decoded['mint'])
        # Make sure decoded policy id correct
        expect(policy_id).to eq SHELLEY.keys.create_policy_id(@wid, policy_script)['policy_id']
        payment = [{ 'address' => src_address,
                     'amount' => { 'quantity' => 0, 'unit' => 'lovelace' },
                     'assets' => [{ 'policy_id' => policy_id,
                                    'asset_name' => assets_name,
                                    'quantity' => assets_quantity }] }]

        _, _, tx_submitted = construct_sign_submit(@target_id, payment)
        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@target_id, tx_id)
        src_after_sending = get_shelley_balances(@wid)
        assets = assets_balance(src_after_sending['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.to_set { |z| { z => assets_quantity } })

        # Burn them on src wallet:
        burn = [burn(assets_name, assets_quantity, policy_script)]
        tx_constructed, _, tx_submitted = construct_sign_submit(@wid,
                                                                nil, # payments
                                                                nil, # withdrawal
                                                                nil, # metadata
                                                                nil, # delegations
                                                                burn)

        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)

        # make sure it was burned
        src_after_burning = get_shelley_balances(@wid)
        assets_to_check = get_assets_from_decode(tx_decoded['burn'])
        assets = assets_balance(src_after_burning['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq({}.to_set)
      end

      ##
      # Make sure minting above boundary quantity values returns proper error
      describe 'Mint/Burn quantities' do
        matrix = [
          [-9_223_372_036_854_775_808, 400, 'bad_request'],
          [-1, 400, 'bad_request'],
          [0, 403, 'mint_or_burn_asset_quantity_out_of_bounds'],
          [9_223_372_036_854_775_808, 403, 'mint_or_burn_asset_quantity_out_of_bounds']
        ]
        matrix.each do |m|
          quantity = m[0]
          code = m[1]
          message = m[2]
          it "Cannot mint #{quantity} assets" do
            policy_script = 'cosigner#0'
            assets_name = asset_name('AmazingNFTIdontHave')
            assets_quantity = quantity
            address = SHELLEY.addresses.list(@wid).first['id']
            mint = [mint(assets_name, assets_quantity, policy_script, address)]
            create_policy_key_if_not_exists(@wid)
            tx_constructed = SHELLEY.transactions.construct(@wid,
                                                            nil, # payments
                                                            nil, # withdrawal
                                                            nil, # metadata
                                                            nil, # delegations
                                                            mint)
            expect(tx_constructed).to be_correct_and_respond code
            expect(tx_constructed['code'].to_s).to eq message
            # expect(tx_constructed['message'].to_s).to include "assetName: #{asset_name('AmazingNFTIdontHave')}"
            # expect(tx_constructed['message'].to_s).to include "quantity: 1"
          end

          it "Cannot burn #{quantity} assets" do
            policy_script = 'cosigner#0'
            assets_name = asset_name('AmazingNFTIdontHave')
            assets_quantity = quantity
            burn = [burn(assets_name, assets_quantity, policy_script)]
            create_policy_key_if_not_exists(@wid)
            tx_constructed = SHELLEY.transactions.construct(@wid,
                                                            nil, # payments
                                                            nil, # withdrawal
                                                            nil, # metadata
                                                            nil, # delegations
                                                            burn)
            expect(tx_constructed).to be_correct_and_respond code
            expect(tx_constructed['code'].to_s).to eq message
          end
        end
      end

      ##
      # Make sure minting above boundary asset_name values returns proper error
      describe 'Mint/Burn asset_name' do
        matrix = [
          ['too long', '1' * 66, 403, 'asset_name_too_long'],
          ['invalid hex', '1', 400, 'bad_request']
        ]
        matrix.each do |m|
          test = m[0]
          assets_name = m[1]
          code = m[2]
          message = m[3]
          it "Cannot mint if my asset name is #{test}" do
            address = SHELLEY.addresses.list(@wid).first['id']
            policy_script = 'cosigner#0'
            assets_quantity = 1500
            mint = [mint(assets_name, assets_quantity, policy_script, address)]
            tx_constructed = SHELLEY.transactions.construct(@wid,
                                                            nil, # payments
                                                            nil, # withdrawal
                                                            nil, # metadata
                                                            nil, # delegations
                                                            mint)

            expect(tx_constructed).to be_correct_and_respond code
            expect(tx_constructed['code']).to eq message
          end
        end
      end

      ##
      # Test against some invalid policy script examples
      describe 'Mint/Burn invalid policy script' do
        matrix = [
          ['cosigner#1', 403, 'created_wrong_policy_script_template'],
          [{ 'all' => ['cosigner#0', 'cosigner#1'] }, 403, 'created_wrong_policy_script_template'],
          [{ 'any' => ['cosigner#0', 'cosigner#0'] }, 403, 'created_wrong_policy_script_template'],
          [{ 'some' => { 'at_least' => 2, 'from' => ['cosigner#0'] } }, 403, 'created_wrong_policy_script_template'],
          [{ 'some' => ['cosigner#0'] }, 400, 'bad_request']
        ]

        matrix.each do |m|
          policy_script = m[0]
          code = m[1]
          message = m[2]
          it "Cannot mint if my policy script is invalid: #{policy_script}" do
            address = SHELLEY.addresses.list(@wid).first['id']
            assets_name = asset_name('WillNotMintIt')
            assets_quantity = 1500
            mint = [mint(assets_name, assets_quantity, policy_script, address)]
            tx_constructed = SHELLEY.transactions.construct(@wid,
                                                            nil, # payments
                                                            nil, # withdrawal
                                                            nil, # metadata
                                                            nil, # delegations
                                                            mint)

            expect(tx_constructed).to be_correct_and_respond code
            expect(tx_constructed['code']).to eq message
          end
        end
      end

      it 'Cannot mint if I make too big transaction' do
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script = 'cosigner#0'
        assets_quantity = 1500

        mint = []
        1000.times do |i|
          mint << mint(asset_name("TooBIG#{i}"), assets_quantity, policy_script, address)
        end
        tx_constructed = SHELLEY.transactions.construct(@wid,
                                                        nil, # payments
                                                        nil, # withdrawal
                                                        nil, # metadata
                                                        nil, # delegations
                                                        mint)

        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code']).to eq 'output_token_bundle_size_exceeds_limit'
      end
    end

    describe 'Output encoding' do
      # Check construct and sign output has desired encoding as per
      # encoding parameter
      matrix = %w[base16 base64]
      matrix.each do |o_enc|
        it o_enc do
          tx_constructed = SHELLEY.transactions.construct(@wid,
                                                          nil, # payments
                                                          nil, # withdrawal
                                                          METADATA,
                                                          nil, # delegations
                                                          nil, # mint
                                                          nil, # validity_interval
                                                          o_enc) # encoding
          expect(tx_constructed).to be_correct_and_respond 202
          expect(method("#{o_enc}?").call(tx_constructed['transaction'])).to be true

          tx_signed = SHELLEY.transactions.sign(@wid, PASS,
                                                tx_constructed['transaction'],
                                                o_enc)
          expect(tx_signed).to be_correct_and_respond 202
          expect(method("#{o_enc}?").call(tx_signed['transaction'])).to be true
        end
      end
    end
  end

  describe 'E2E Shelley' do
    describe 'UTxOs' do
      it 'Fixture shelley wallet has utxos' do
        utxo_stats = SHELLEY.wallets.utxo(@wid)
        expect(utxo_stats).to be_correct_and_respond 200

        utxo_snap = SHELLEY.wallets.utxo_snapshot(@wid)
        expect(utxo_snap).to be_correct_and_respond 200
        expect(utxo_snap['entries'].size).to be > 0
      end
    end

    describe 'Native Assets' do
      it 'I can list native assets' do
        assets = SHELLEY.assets.get @wid
        expect(assets).to be_correct_and_respond 200
        expect(assets.to_s).to include ASSETS[0]['policy_id']
        expect(assets.to_s).to include ASSETS[0]['asset_name']
        expect(assets.to_s).to include ASSETS[1]['policy_id']
        expect(assets.to_s).to include ASSETS[1]['asset_name']

        assets = SHELLEY.assets.get(@wid, ASSETS[0]['policy_id'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[0]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[0]['asset_name']
        expect(assets['asset_name']).not_to eq ASSETS[1]['asset_name']

        assets = SHELLEY.assets.get(@wid, ASSETS[1]['policy_id'],
                                    ASSETS[1]['asset_name'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[1]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[1]['asset_name']
        expect(assets['asset_name']).not_to eq ASSETS[0]['asset_name']
      end

      it 'I can list native assets and get offchain metadata', :offchain do
        assets = SHELLEY.assets.get @wid
        expect(assets).to be_correct_and_respond 200
        expect(assets.to_s).to include ASSETS[0]['policy_id']
        expect(assets.to_s).to include ASSETS[0]['asset_name']
        expect(assets.to_s).to include ASSETS[0]['metadata']['name']
        expect(assets.to_s).to include ASSETS[1]['policy_id']
        expect(assets.to_s).to include ASSETS[1]['asset_name']
        expect(assets.to_s).to include ASSETS[1]['metadata']['name']

        assets = SHELLEY.assets.get(@wid, ASSETS[0]['policy_id'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[0]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[0]['asset_name']
        expect(assets['metadata']).to eq ASSETS[0]['metadata']
        expect(assets['asset_name']).not_to eq ASSETS[1]['asset_name']
        expect(assets['metadata']).not_to eq ASSETS[1]['metadata']

        assets = SHELLEY.assets.get(@wid, ASSETS[1]['policy_id'],
                                    ASSETS[1]['asset_name'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[1]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[1]['asset_name']
        expect(assets['metadata']).to eq ASSETS[1]['metadata']
        expect(assets['asset_name']).not_to eq ASSETS[0]['asset_name']
        expect(assets['metadata']).not_to eq ASSETS[0]['metadata']['name']
      end

      it 'I can send native assets tx and they are received' do
        amt = 1
        address = SHELLEY.addresses.list(@target_id)[1]['id']
        target_before = get_shelley_balances(@target_id)

        payload = [{ 'address' => address,
                     'amount' => { 'quantity' => 0, 'unit' => 'lovelace' },
                     'assets' => [{ 'policy_id' => ASSETS[0]['policy_id'],
                                    'asset_name' => ASSETS[0]['asset_name'],
                                    'quantity' => amt },
                                  { 'policy_id' => ASSETS[1]['policy_id'],
                                    'asset_name' => ASSETS[1]['asset_name'],
                                    'quantity' => amt }] }]

        tx_sent = SHELLEY.transactions.create(@wid, PASS, payload)

        expect(tx_sent).to be_correct_and_respond 202
        wait_for_tx_in_ledger(@wid, tx_sent['id'])
        verify_tx_status(@wid, tx_sent['id'], 'in_ledger')

        target_after = get_shelley_balances(@target_id)

        # verify balances are correct on target wallet
        assets_to_check = ["#{ASSETS[0]['policy_id']}#{ASSETS[0]['asset_name']}",
                           "#{ASSETS[1]['policy_id']}#{ASSETS[1]['asset_name']}"]
        target_total_after = assets_balance(target_after['assets_total'], { assets_to_check: assets_to_check })
        target_avail_after = assets_balance(target_after['assets_available'], { assets_to_check: assets_to_check })
        target_total_expected = assets_balance(target_before['assets_total'], { assets_to_check: assets_to_check, delta: +amt })
        target_avail_expected = assets_balance(target_before['assets_available'], { assets_to_check: assets_to_check, delta: +amt })
        if target_before['assets_total'] == []
          target_balance_expected = assets_to_check.to_set { |a| { a => amt } }
          expect(target_total_after).to eq target_balance_expected
          expect(target_avail_after).to eq target_balance_expected
        else
          expect(target_total_after).to eq target_total_expected
          expect(target_avail_after).to eq target_avail_expected
        end
      end
    end

    describe 'Shelley Migrations' do
      it 'I can create migration plan shelley -> shelley' do
        addrs = SHELLEY.addresses.list(@target_id).map { |a| a['id'] }

        plan = SHELLEY.migrations.plan(@wid, addrs)
        expect(plan).to be_correct_and_respond 202
        expect(plan['balance_selected']['assets']).not_to be []
        expect(plan['balance_leftover']).to eq({ 'ada' => { 'quantity' => 0,
                                                            'unit' => 'lovelace' },
                                                 'assets' => [] })
      end
    end

    describe 'Shelley Transactions' do
      it 'I can send transaction and funds are received (min_utxo_value)' do
        address = SHELLEY.addresses.list(@target_id)[0]['id']
        available_before = SHELLEY.wallets.get(@target_id)['balance']['available']['quantity']
        total_before = SHELLEY.wallets.get(@target_id)['balance']['total']['quantity']

        # get required minimum ada
        tx = SHELLEY.transactions.create(@wid, PASS, payment_payload(1, address))
        expect(tx.code).to eq 403
        expect(tx['code']).to eq 'utxo_too_small'
        amt = tx['info']['tx_output_lovelace_required_minimum']['quantity']

        tx_sent = SHELLEY.transactions.create(@wid, PASS, payment_payload(amt, address))

        expect(tx_sent).to be_correct_and_respond 202
        tx_id = tx_sent['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        verify_tx_status(@wid, tx_id, 'in_ledger')

        eventually "Funds are on target wallet: #{@target_id}" do
          available = SHELLEY.wallets.get(@target_id)['balance']['available']['quantity']
          total = SHELLEY.wallets.get(@target_id)['balance']['total']['quantity']
          (available == amt + available_before) && (total == amt + total_before)
        end

        # examine the tx in history
        # on src wallet
        tx = SHELLEY.transactions.get(@wid, tx_id)
        expect(tx['amount']['quantity']).to be > amt
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'outgoing')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, nil)
        tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(tx, present: false)
        tx_mint_burn(tx, mint: [], burn: [])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: false)

        # on target wallet
        txt = SHELLEY.transactions.get(@target_id, tx_id)
        tx_amount(txt, amt)
        # in old tx workflow, only expecting fee to be present because fee calculation is not reliable enough for strong assertions
        expect(tx['fee']['quantity'].to_i).to be > 0
        tx_inputs(txt, present: true)
        tx_outputs(txt, present: true)
        tx_direction(txt, 'incoming')
        tx_script_validity(txt, 'valid')
        tx_status(txt, 'in_ledger')
        tx_collateral(txt, present: false)
        tx_collateral_outputs(txt, present: false)
        tx_metadata(txt, nil)
        # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
        # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(txt, present: false)
        tx_mint_burn(txt, mint: [], burn: [])
        tx_extra_signatures(txt, present: false)
        tx_script_integrity(txt, present: false)
        tx_validity_interval_default(txt)
        tx_certificates(txt, present: false)
      end

      it 'I can send transaction with ttl and funds are received' do
        amt = MIN_UTXO_VALUE_PURE_ADA
        ttl_in_s = 1200

        address = SHELLEY.addresses.list(@target_id)[0]['id']
        target_before = get_shelley_balances(@target_id)
        tx_sent = SHELLEY.transactions.create(@wid,
                                              PASS,
                                              [{ address => amt }],
                                              nil, # withdrawal
                                              nil, # metadata
                                              ttl_in_s)

        expect(tx_sent).to be_correct_and_respond 202
        tx_id = tx_sent['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        verify_tx_status(@wid, tx_id, 'in_ledger')

        target_after = get_shelley_balances(@target_id)

        expect(target_after['available']).to eq(amt + target_before['available'])
        expect(target_after['total']).to eq(amt + target_before['total'])
        expect(target_after['reward']).to eq(target_before['reward'])

        # examine the tx in history
        # on src wallet
        tx = SHELLEY.transactions.get(@wid, tx_id)
        expect(tx['amount']['quantity']).to be > amt
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'outgoing')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, nil)
        tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(tx, present: false)
        tx_mint_burn(tx, mint: [], burn: [])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: false)

        # on target wallet
        txt = SHELLEY.transactions.get(@target_id, tx_id)
        tx_amount(txt, amt)
        expect(tx['fee']['quantity'].to_i).to be > 0
        tx_inputs(txt, present: true)
        tx_outputs(txt, present: true)
        tx_direction(txt, 'incoming')
        tx_script_validity(txt, 'valid')
        tx_status(txt, 'in_ledger')
        tx_collateral(txt, present: false)
        tx_collateral_outputs(txt, present: false)
        tx_metadata(txt, nil)
        # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
        # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(txt, present: false)
        tx_mint_burn(txt, mint: [], burn: [])
        tx_extra_signatures(txt, present: false)
        tx_script_integrity(txt, present: false)
        tx_validity_interval_default(txt)
        tx_certificates(txt, present: false)
      end

      it 'Transaction with ttl = 0 would expire and I can forget it' do
        skip 'Test is flaky due to ADP-608'
        amt = MIN_UTXO_VALUE_PURE_ADA
        ttl_in_s = 0

        address = SHELLEY.addresses.list(@target_id)[0]['id']
        tx_sent = SHELLEY.transactions.create(@wid,
                                              PASS,
                                              [{ address => amt }],
                                              nil, # withdrawal
                                              nil, # metadata
                                              ttl_in_s)

        expect(tx_sent).to be_correct_and_respond 202

        eventually "TX `#{tx_sent['id']}' expires on `#{@wid}'" do
          SHELLEY.transactions.get(@wid, tx_sent['id'])['status'] == 'expired'
        end

        res = SHELLEY.transactions.forget(@wid, tx_sent['id'])
        expect(res).to be_correct_and_respond 204

        fres = SHELLEY.transactions.get(@wid, tx_sent['id'])
        expect(fres).to be_correct_and_respond 404
      end

      it "I can send transaction using 'withdrawal' flag and funds are received" do
        amt = MIN_UTXO_VALUE_PURE_ADA
        address = SHELLEY.addresses.list(@target_id)[0]['id']
        target_before = get_shelley_balances(@target_id)
        src_before = get_shelley_balances(@wid)

        tx_sent = SHELLEY.transactions.create(@wid, PASS, [{ address => amt }], 'self')

        expect(tx_sent).to be_correct_and_respond 202
        tx_id = tx_sent['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        verify_tx_status(@wid, tx_id, 'in_ledger')

        fee = SHELLEY.transactions.get(@wid, tx_id)['fee']['quantity']
        target_after = get_shelley_balances(@target_id)
        src_after = get_shelley_balances(@wid)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt, fee)

        # examine the tx in history
        # on src wallet
        tx = SHELLEY.transactions.get(@wid, tx_id)
        expect(tx['amount']['quantity']).to be > amt
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'outgoing')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, nil)
        tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(tx, present: false)
        tx_mint_burn(tx, mint: [], burn: [])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: false)

        # on target wallet
        txt = SHELLEY.transactions.get(@target_id, tx_id)
        tx_amount(txt, amt)
        expect(tx['fee']['quantity'].to_i).to be > 0
        tx_inputs(txt, present: true)
        tx_outputs(txt, present: true)
        tx_direction(txt, 'incoming')
        tx_script_validity(txt, 'valid')
        tx_status(txt, 'in_ledger')
        tx_collateral(txt, present: false)
        tx_collateral_outputs(txt, present: false)
        tx_metadata(txt, nil)
        # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
        # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(txt, present: false)
        tx_mint_burn(txt, mint: [], burn: [])
        tx_extra_signatures(txt, present: false)
        tx_script_integrity(txt, present: false)
        tx_validity_interval_default(txt)
        tx_certificates(txt, present: false)
      end

      it 'I can send transaction with metadata' do
        amt = MIN_UTXO_VALUE_PURE_ADA
        metadata = METADATA

        address = SHELLEY.addresses.list(@target_id)[0]['id']
        target_before = get_shelley_balances(@target_id)

        tx_sent = SHELLEY.transactions.create(@wid,
                                              PASS,
                                              [{ address => amt }],
                                              nil,
                                              metadata)

        expect(tx_sent).to be_correct_and_respond 202
        tx_id = tx_sent['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        verify_tx_status(@wid, tx_id, 'in_ledger')

        target_after = get_shelley_balances(@target_id)

        expect(target_after['available']).to eq(amt + target_before['available'])
        expect(target_after['total']).to eq(amt + target_before['total'])
        expect(target_after['reward']).to eq(target_before['reward'])

        # examine the tx in history
        # on src wallet
        tx = SHELLEY.transactions.get(@wid, tx_id)
        expect(tx['amount']['quantity']).to be > amt
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'outgoing')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, metadata)
        tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(tx, present: false)
        tx_mint_burn(tx, mint: [], burn: [])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: false)

        # on target wallet
        txt = SHELLEY.transactions.get(@target_id, tx_id)
        tx_amount(txt, amt)
        expect(tx['fee']['quantity'].to_i).to be > 0
        tx_inputs(txt, present: true)
        tx_outputs(txt, present: true)
        tx_direction(txt, 'incoming')
        tx_script_validity(txt, 'valid')
        tx_status(txt, 'in_ledger')
        tx_collateral(txt, present: false)
        tx_collateral_outputs(txt, present: false)
        tx_metadata(txt, metadata)
        # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
        # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(txt, present: false)
        tx_mint_burn(txt, mint: [], burn: [])
        tx_extra_signatures(txt, present: false)
        tx_script_integrity(txt, present: false)
        tx_validity_interval_default(txt)
        tx_certificates(txt, present: false)
      end

      it 'I can estimate fee (min_utxo_value)' do
        metadata = METADATA
        txs = SHELLEY.transactions
        # get required minimum ada
        fees = txs.payment_fees(@wid, payment_payload(1))
        expect(fees.code).to eq 403
        expect(fees['code']).to eq 'utxo_too_small'
        amt = fees['info']['tx_output_lovelace_required_minimum']['quantity']

        fees = txs.payment_fees(@wid, payment_payload(amt))
        expect(fees).to be_correct_and_respond 202

        fees = txs.payment_fees(@wid, payment_payload(amt), 'self')
        expect(fees).to be_correct_and_respond 202

        fees = txs.payment_fees(@wid, payment_payload(amt), 'self', metadata)
        expect(fees).to be_correct_and_respond 202
      end

      it 'I can list transactions and limit response with query parameters' do
        wid = @wid

        # get 3 txs
        txs = SHELLEY.transactions.list(wid, { max_count: 3, order: 'ascending' })
        expect(txs).to be_correct_and_respond 200
        expect(txs.size).to be 3

        last_tx_time =  txs.first['inserted_at']['time']
        first_tx_time = txs.last['inserted_at']['time']

        # get 2 txs
        txs = SHELLEY.transactions.list(wid, { max_count: 2, order: 'ascending' })
        expect(txs).to be_correct_and_respond 200
        expect(txs.size).to eq 2
        expect(txs.first['inserted_at']['time']).to eq last_tx_time

        # get 2 txs in ascending order
        txs = SHELLEY.transactions.list(wid, { max_count: 2, start: first_tx_time, order: 'ascending' })
        expect(txs).to be_correct_and_respond 200
        expect(txs.size).to eq 2
        expect(txs.first['inserted_at']['time']).to eq first_tx_time

        # get 2 txs in ascending order with start and end time
        txs = SHELLEY.transactions.list(wid, { max_count: 2, start: last_tx_time, end: first_tx_time, order: 'ascending' })
        expect(txs).to be_correct_and_respond 200
        expect(txs.size).to eq 2
        expect(txs.first['inserted_at']['time']).to eq last_tx_time
      end
    end

    describe 'Stake Pools' do
      it "I could join Stake Pool - if I knew it's id" do
        pools = SHELLEY.stake_pools

        join = pools.join(SPID, @wid, PASS)
        expect(join).to be_correct_and_respond 404
        expect(join.to_s).to include 'no_such_pool'
      end

      it 'I could check delegation fees - if I could cover fee' do
        id = create_shelley_wallet

        pools = SHELLEY.stake_pools
        fees = pools.delegation_fees(id)
        expect(fees).to be_correct_and_respond 403
        expect(fees.to_s).to include 'not_enough_money'
      end

      it 'I could join Stake Pool - if I had enough to cover fee' do
        id = create_shelley_wallet
        pools = SHELLEY.stake_pools
        pool_id = pools.list({ stake: 1000 })[0]['id']

        join = pools.join(pool_id, id, PASS)
        expect(join).to be_correct_and_respond 403
        expect(join.to_s).to include 'not_enough_money'
      end

      it 'Can list stake pools only when stake is provided' do
        pools = SHELLEY.stake_pools
        l = pools.list({ stake: 1000 })
        l_bad = pools.list
        expect(l).to be_correct_and_respond 200

        expect(l_bad).to be_correct_and_respond 400
        expect(l_bad.to_s).to include 'query_param_missing'
      end

      it 'Can join and quit Stake Pool' do
        skip 'ADP-3243'
        # Get funds on the wallet
        address = SHELLEY.addresses.list(@target_id)[0]['id']
        amt = 10_000_000
        deposit = CARDANO_CLI.protocol_params['stakeAddressDeposit']
        tx_sent = SHELLEY.transactions.create(@wid,
                                              PASS,
                                              [{ address => amt }])

        expect(tx_sent).to be_correct_and_respond 202
        wait_for_tx_in_ledger(@wid, tx_sent['id'])
        verify_tx_status(@wid, tx_sent['id'], 'in_ledger')

        stake = get_shelley_balances(@target_id)['total']

        # Check wallet stake keys before joing stake pool
        stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
        expect(stake_keys).to be_correct_and_respond 200
        expect(stake_keys['foreign'].size).to eq 0
        expect(stake_keys['ours'].size).to eq 1
        expect(stake_keys['ours'].first['stake']['quantity']).to eq stake
        expect(stake_keys['none']['stake']['quantity']).to eq 0
        expect(stake_keys['ours'].first['delegation']['active']['status']).to eq 'not_delegating'
        # expect(stake_keys['ours'].first['delegation']['next']).to eq []

        # Pick up pool id to join
        pools = SHELLEY.stake_pools
        pool_id = pools.list({ stake: 1000 }).sample['id']

        # Join pool
        puts "Joining pool: #{pool_id}"
        join = pools.join(pool_id, @target_id, PASS)

        expect(join).to be_correct_and_respond 202
        expect(join.to_s).to include 'status'

        join_tx_id = join['id']
        eventually "Checking if join tx id (#{join_tx_id}) is in_ledger" do
          tx = SHELLEY.transactions.get(@target_id, join_tx_id)
          tx['status'] == 'in_ledger'
        end
        tx = SHELLEY.transactions.get(@target_id, join_tx_id)
        fee = tx['fee']['quantity']

        stake_after_joining = stake - deposit - fee

        # Check wallet stake keys after joing stake pool
        stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
        expect(stake_keys).to be_correct_and_respond 200
        expect(stake_keys['foreign'].size).to eq 0
        expect(stake_keys['ours'].size).to eq 1
        expect(stake_keys['ours'].first['stake']['quantity']).to eq stake_after_joining
        expect(stake_keys['none']['stake']['quantity']).to eq 0
        expect(stake_keys['ours'].first['delegation']['active']['status']).to eq 'not_delegating'
        expect(stake_keys['ours'].first['delegation']['next'].last['status']).to eq 'delegating'

        # examine the tx in history
        tx_amount(tx, fee + deposit)
        tx_fee(tx, fee)
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'outgoing')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, nil)
        tx_deposits(tx, deposit_taken: deposit, deposit_returned: 0)
        tx_withdrawals(tx, present: false)
        tx_mint_burn(tx, mint: [], burn: [])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: true)
        expect(tx['certificates'].to_s).to include 'register_reward_account'
        expect(tx['certificates'].to_s).to include 'join_pool'
        expect(tx['certificates'].to_s).to include pool_id

        # Quit pool
        puts "Quitting pool: #{pool_id}"
        quit = pools.quit(@target_id, PASS)

        expect(quit).to be_correct_and_respond 202
        expect(quit.to_s).to include 'status'

        quit_tx_id = quit['id']
        eventually "Checking if quit tx id (#{quit_tx_id}) is in_ledger" do
          tx = SHELLEY.transactions.get(@target_id, quit_tx_id)
          tx['status'] == 'in_ledger'
        end
        total_balance_after = get_shelley_balances(@target_id)['total']

        # Check wallet stake keys after quitting stake pool
        stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
        expect(stake_keys).to be_correct_and_respond 200
        expect(stake_keys['foreign'].size).to eq 0
        expect(stake_keys['ours'].size).to eq 1
        # deposit is back on quitting so stake is higher than before
        expect(stake_keys['ours'].first['stake']['quantity']).to be > stake_after_joining
        expect(stake_keys['ours'].first['stake']['quantity']).to eq total_balance_after
        expect(stake_keys['none']['stake']['quantity']).to eq 0
        expect(stake_keys['ours'].first['delegation']['active']['status']).to eq 'not_delegating'
        expect(stake_keys['ours'].first['delegation']['next'].last['status']).to eq 'not_delegating'

        # examine the tx in history
        tx = SHELLEY.transactions.get(@target_id, quit_tx_id)
        expect(tx['amount']['quantity']).to be > 0
        expect(tx['amount']['quantity']).to be < deposit
        expect(tx['fee']['quantity'].to_i).to be > 0
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'incoming')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, nil)
        tx_deposits(tx, deposit_taken: 0, deposit_returned: deposit)
        tx_withdrawals(tx, present: true) # automatic withdrawal
        tx_mint_burn(tx, mint: [], burn: [])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: true)
        expect(tx['certificates'].to_s).to include 'quit_pool'
      end
    end

    describe 'Coin Selection' do
      it 'I can trigger random coin selection' do
        addresses = SHELLEY.addresses.list(@target_id)
        payload = [{ 'address' => addresses[0]['id'],
                     'amount' => { 'quantity' => 2_000_000, 'unit' => 'lovelace' },
                     'assets' => [{ 'policy_id' => ASSETS[0]['policy_id'],
                                    'asset_name' => ASSETS[0]['asset_name'],
                                    'quantity' => 1 },
                                  { 'policy_id' => ASSETS[1]['policy_id'],
                                    'asset_name' => ASSETS[1]['asset_name'],
                                    'quantity' => 1 }] }]

        rnd = SHELLEY.coin_selections.random(@wid, payload, 'self', METADATA)

        expect(rnd).to be_correct_and_respond 200
        expect(rnd.to_s).to include 'outputs'
        expect(rnd.to_s).to include 'change'
        expect(rnd.to_s).to include 'metadata'
        expect(rnd['inputs']).not_to be_empty
        expect(rnd['outputs']).not_to be_empty
      end

      it 'I can trigger random coin selection delegation action' do
        pid = SHELLEY.stake_pools.list({ stake: 10_000_000 }).sample['id']
        action_join = { action: 'join', pool: pid }

        rnd = SHELLEY.coin_selections.random_deleg @wid, action_join

        expect(rnd).to be_correct_and_respond 200
        expect(rnd.to_s).to include 'outputs'
        expect(rnd.to_s).to include 'change'
        expect(rnd['inputs']).not_to be_empty
        expect(rnd['change']).not_to be_empty
        expect(rnd['outputs']).to be_empty
        expect(rnd['certificates']).not_to be_empty
        # expect(rnd['certificates'].to_s).to include "register_reward_account"
        expect(rnd['certificates'].to_s).to include 'join_pool'
      end

      it 'I could trigger random coin selection delegation action - if I had money' do
        wid = create_shelley_wallet
        pid = SHELLEY.stake_pools.list({ stake: 10_000_000 }).sample['id']
        action_join = { action: 'join', pool: pid }

        rnd = SHELLEY.coin_selections.random_deleg wid, action_join
        expect(rnd).to be_correct_and_respond 403
        expect(rnd.to_s).to include 'not_enough_money'
      end

      it 'I could trigger random coin selection delegation action - if I known pool id' do
        action_join = { action: 'join', pool: SPID_BECH32 }
        action_quit = { action: 'quit' }

        rnd = SHELLEY.coin_selections.random_deleg @wid, action_join
        expect(rnd).to be_correct_and_respond 404
        expect(rnd.to_s).to include 'no_such_pool'

        rnd = SHELLEY.coin_selections.random_deleg @wid, action_quit
        expect(rnd).to be_correct_and_respond 403
        expect(rnd.to_s).to include 'not_delegating_to'
      end
    end

    describe 'Update passphrase' do
      it 'I can update passphrase with mnemonic and the wallet does not have to re-sync' do
        mnemonics = get_fixture_wallet(:fixture, :shelley, :mnemonics)
        upd = SHELLEY.wallets.update_passphrase(@wid, { mnemonic_sentence: mnemonics,
                                                        new_passphrase: PASS })
        expect(upd).to be_correct_and_respond 204
        wallet = SHELLEY.wallets.get(@wid)
        expect(wallet['state']['status']).to eq 'ready'
      end
    end
  end

  describe 'E2E Byron' do
    def test_byron_fees(source_wid)
      txs = BYRON.transactions
      # get required minimum ada
      fees = txs.payment_fees(source_wid, payment_payload(1))
      expect(fees.code).to eq 403
      expect(fees['code']).to eq 'utxo_too_small'
      amt = fees['info']['tx_output_lovelace_required_minimum']['quantity']

      fees = txs.payment_fees(source_wid, payment_payload(amt))
      expect(fees).to be_correct_and_respond 202
    end

    def test_byron_tx(source_wid, target_wid)
      address = SHELLEY.addresses.list(target_wid)[0]['id']
      target_before = get_shelley_balances(target_wid)
      src_before = get_byron_balances(source_wid)

      # get required minimum ada
      tx = BYRON.transactions.create(source_wid, PASS, payment_payload(1, address))
      expect(tx.code).to eq 403
      expect(tx['code']).to eq 'utxo_too_small'
      amt = tx['info']['tx_output_lovelace_required_minimum']['quantity']

      tx_sent = BYRON.transactions.create(source_wid, PASS, payment_payload(amt, address))

      expect(tx_sent).to be_correct_and_respond 202
      tx_id = tx_sent['id']
      wait_for_tx_in_ledger(target_wid, tx_id)
      verify_tx_status(source_wid, tx_id, 'in_ledger', BYRON)

      target_after = get_shelley_balances(target_wid)
      src_after = get_byron_balances(source_wid)
      fee = BYRON.transactions.get(source_wid, tx_id)['fee']['quantity']

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amt, fee)

      # examine the tx in history
      # on src wallet
      tx = BYRON.transactions.get(source_wid, tx_id)
      tx_amount(tx, amt + fee)
      tx_fee(tx, fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: false)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: false)

      # on target wallet
      txt = SHELLEY.transactions.get(target_wid, tx_id)
      tx_amount(txt, amt)
      tx_fee(tx, fee)
      tx_inputs(txt, present: true)
      tx_outputs(txt, present: true)
      tx_direction(txt, 'incoming')
      tx_script_validity(txt, 'valid')
      tx_status(txt, 'in_ledger')
      tx_collateral(txt, present: false)
      tx_collateral_outputs(txt, present: false)
      tx_metadata(txt, nil)
      # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
      # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(txt, present: false)
      tx_mint_burn(txt, mint: [], burn: [])
      tx_extra_signatures(txt, present: false)
      tx_script_integrity(txt, present: false)
      tx_validity_interval_default(txt)
      tx_certificates(txt, present: false)
    end

    def test_byron_assets_tx(source_id, target_id)
      amt = 1
      address = SHELLEY.addresses.list(target_id)[1]['id']
      target_before = get_shelley_balances(target_id)
      src_before = get_byron_balances(source_id)
      payload = [{ 'address' => address,
                   'amount' => { 'quantity' => 0, 'unit' => 'lovelace' },
                   'assets' => [{ 'policy_id' => ASSETS[0]['policy_id'],
                                  'asset_name' => ASSETS[0]['asset_name'],
                                  'quantity' => amt },
                                { 'policy_id' => ASSETS[1]['policy_id'],
                                  'asset_name' => ASSETS[1]['asset_name'],
                                  'quantity' => amt }] }]

      tx_sent = BYRON.transactions.create(source_id, PASS, payload)

      expect(tx_sent).to be_correct_and_respond 202
      tx_id = tx_sent['id']
      wait_for_tx_in_ledger(target_id, tx_id)
      verify_tx_status(source_id, tx_id, 'in_ledger', BYRON)

      target_after = get_shelley_balances(target_id)
      src_after = get_byron_balances(source_id)
      tx = BYRON.transactions.get(source_id, tx_id)
      fee = tx['fee']['quantity']
      amt_ada = tx['amount']['quantity'] - fee

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amt_ada, fee)

      verify_asset_balance(src_after, src_before,
                           target_after, target_before,
                           amt)

      # Target wallet lists my associated assets
      assets = SHELLEY.assets.get(target_id)
      expect(assets).to be_correct_and_respond 200
      expect(assets.to_s).to include ASSETS[0]['policy_id']
      expect(assets.to_s).to include ASSETS[0]['asset_name']
      expect(assets.to_s).to include ASSETS[1]['policy_id']
      expect(assets.to_s).to include ASSETS[1]['asset_name']

      # examine the tx in history
      # on src wallet
      tx = BYRON.transactions.get(source_id, tx_id)
      tx_amount(tx, amt_ada + fee)
      tx_fee(tx, fee)
      tx_inputs(tx, present: true)
      tx_outputs(tx, present: true)
      tx_direction(tx, 'outgoing')
      tx_script_validity(tx, 'valid')
      tx_status(tx, 'in_ledger')
      tx_collateral(tx, present: false)
      tx_collateral_outputs(tx, present: false)
      tx_metadata(tx, nil)
      tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(tx, present: false)
      tx_mint_burn(tx, mint: [], burn: [])
      tx_extra_signatures(tx, present: false)
      tx_script_integrity(tx, present: false)
      tx_validity_interval_default(tx)
      tx_certificates(tx, present: false)

      # on target wallet
      txt = SHELLEY.transactions.get(target_id, tx_id)
      tx_amount(txt, amt_ada)
      tx_fee(tx, fee)
      tx_inputs(txt, present: true)
      tx_outputs(txt, present: true)
      tx_direction(txt, 'incoming')
      tx_script_validity(txt, 'valid')
      tx_status(txt, 'in_ledger')
      tx_collateral(txt, present: false)
      tx_collateral_outputs(txt, present: false)
      tx_metadata(txt, nil)
      # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
      # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
      tx_withdrawals(txt, present: false)
      tx_mint_burn(txt, mint: [], burn: [])
      tx_extra_signatures(txt, present: false)
      tx_script_integrity(txt, present: false)
      tx_validity_interval_default(txt)
      tx_certificates(txt, present: false)
    end

    def test_byron_trans_list(wid)
      # get 3 txs
      txs = BYRON.transactions.list(wid, { max_count: 3, order: 'ascending' })
      expect(txs).to be_correct_and_respond 200
      expect(txs.size).to be 3

      last_tx_time =  txs.first['inserted_at']['time']
      first_tx_time = txs.last['inserted_at']['time']

      # get 2 txs
      txs = BYRON.transactions.list(wid, { max_count: 2, order: 'ascending' })
      expect(txs).to be_correct_and_respond 200
      expect(txs.size).to eq 2
      expect(txs.first['inserted_at']['time']).to eq last_tx_time

      # get 2 txs in ascending order
      txs = BYRON.transactions.list(wid, { max_count: 2, start: first_tx_time, order: 'ascending' })
      expect(txs).to be_correct_and_respond 200
      expect(txs.size).to eq 2
      expect(txs.first['inserted_at']['time']).to eq first_tx_time

      # get 2 txs in ascending order with start and end time
      txs = BYRON.transactions.list(wid, { max_count: 2, start: last_tx_time, end: first_tx_time, order: 'ascending' })
      expect(txs).to be_correct_and_respond 200
      expect(txs.size).to eq 2
      expect(txs.first['inserted_at']['time']).to eq last_tx_time
    end

    describe 'UTxOs' do
      it 'Fixture shared wallets have utxos' do
        @nightly_byron_wallets.each do |wid|
          utxo_stats = BYRON.wallets.utxo(wid)
          expect(utxo_stats).to be_correct_and_respond 200

          utxo_snap = BYRON.wallets.utxo_snapshot(wid)
          expect(utxo_snap).to be_correct_and_respond 200
          expect(utxo_snap['entries'].size).to be > 0
        end
      end
    end
    describe 'Byron Transactions' do
      it 'I can estimate fees (min_utxo_value), random' do
        test_byron_fees(@wid_rnd)
      end

      it 'I can estimate fees (min_utxo_value), icarus' do
        test_byron_fees(@wid_ic)
      end

      it 'I can send transaction and funds are received (min_utxo_value), random -> shelley' do
        test_byron_tx(@wid_rnd, @target_id)
      end

      it 'I can send transaction and funds are received (min_utxo_value), icarus -> shelley' do
        test_byron_tx(@wid_ic, @target_id)
      end

      it 'I can send native assets tx and they are received (random -> shelley)' do
        test_byron_assets_tx(@wid_rnd, @target_id)
      end

      it 'I can send native assets tx and they are received (icarus -> shelley)' do
        test_byron_assets_tx(@wid_ic, @target_id)
      end

      it 'I can list transactions and limit response with query parameters (byron)' do
        test_byron_trans_list(@wid_rnd)
      end

      it 'I can list transactions and limit response with query parameters (icarus)' do
        test_byron_trans_list(@wid_ic)
      end
    end

    describe 'Byron Migrations' do
      it 'I can create migration plan byron -> shelley' do
        addrs = SHELLEY.addresses.list(@target_id).map { |a| a['id'] }

        plan = BYRON.migrations.plan(@wid_rnd, addrs)
        expect(plan).to be_correct_and_respond 202
        expect(plan['balance_selected']['assets']).not_to be []
        expect(plan['balance_leftover']).to eq({ 'ada' => { 'quantity' => 0,
                                                            'unit' => 'lovelace' },
                                                 'assets' => [] })
      end

      it 'I can create migration plan icarus -> shelley' do
        addrs = SHELLEY.addresses.list(@target_id).map { |a| a['id'] }

        plan = BYRON.migrations.plan(@wid_ic, addrs)
        expect(plan).to be_correct_and_respond 202
        expect(plan['balance_selected']['assets']).not_to be []
        expect(plan['balance_leftover']).to eq({ 'ada' => { 'quantity' => 0,
                                                            'unit' => 'lovelace' },
                                                 'assets' => [] })
      end
    end

    describe 'Native Assets' do
      it 'I can list assets -> random' do
        assets = BYRON.assets.get @wid_rnd
        expect(assets).to be_correct_and_respond 200
        expect(assets.to_s).to include ASSETS[0]['policy_id']
        expect(assets.to_s).to include ASSETS[0]['asset_name']
        expect(assets.to_s).to include ASSETS[1]['policy_id']
        expect(assets.to_s).to include ASSETS[1]['asset_name']

        assets = BYRON.assets.get(@wid_rnd, ASSETS[0]['policy_id'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[0]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[0]['asset_name']
        expect(assets['asset_name']).not_to eq ASSETS[1]['asset_name']

        assets = BYRON.assets.get(@wid_rnd, ASSETS[1]['policy_id'], ASSETS[1]['asset_name'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[1]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[1]['asset_name']
        expect(assets['asset_name']).not_to eq ASSETS[0]['asset_name']
      end

      it 'I can list assets -> icarus' do
        assets = BYRON.assets.get @wid_ic
        expect(assets).to be_correct_and_respond 200
        expect(assets.to_s).to include ASSETS[0]['policy_id']
        expect(assets.to_s).to include ASSETS[0]['asset_name']
        expect(assets.to_s).to include ASSETS[1]['policy_id']
        expect(assets.to_s).to include ASSETS[1]['asset_name']

        assets = BYRON.assets.get(@wid_ic, ASSETS[0]['policy_id'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[0]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[0]['asset_name']
        expect(assets['asset_name']).not_to eq ASSETS[1]['asset_name']

        assets = BYRON.assets.get(@wid_ic, ASSETS[1]['policy_id'], ASSETS[1]['asset_name'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[1]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[1]['asset_name']
        expect(assets['asset_name']).not_to eq ASSETS[0]['asset_name']
      end

      it 'I can list assets with offchain metadata -> random', :offchain do
        assets = BYRON.assets.get(@wid_rnd, ASSETS[0]['policy_id'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[0]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[0]['asset_name']
        expect(assets['metadata']).to eq ASSETS[0]['metadata']
        expect(assets['asset_name']).not_to eq ASSETS[1]['asset_name']
        expect(assets['metadata']).not_to eq ASSETS[1]['metadata']

        assets = BYRON.assets.get(@wid_rnd, ASSETS[1]['policy_id'], ASSETS[1]['asset_name'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[1]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[1]['asset_name']
        expect(assets['metadata']).to eq ASSETS[1]['metadata']
        expect(assets['asset_name']).not_to eq ASSETS[0]['asset_name']
        expect(assets['metadata']).not_to eq ASSETS[0]['metadata']['name']
      end

      it 'I can list assets with offchain metadata -> icarus', :offchain do
        assets = BYRON.assets.get(@wid_ic, ASSETS[0]['policy_id'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[0]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[0]['asset_name']
        expect(assets['metadata']).to eq ASSETS[0]['metadata']
        expect(assets['asset_name']).not_to eq ASSETS[1]['asset_name']
        expect(assets['metadata']).not_to eq ASSETS[1]['metadata']

        assets = BYRON.assets.get(@wid_ic, ASSETS[1]['policy_id'], ASSETS[1]['asset_name'])
        expect(assets).to be_correct_and_respond 200
        expect(assets['policy_id']).to eq ASSETS[1]['policy_id']
        expect(assets['asset_name']).to eq ASSETS[1]['asset_name']
        expect(assets['metadata']).to eq ASSETS[1]['metadata']
        expect(assets['asset_name']).not_to eq ASSETS[0]['asset_name']
        expect(assets['metadata']).not_to eq ASSETS[0]['metadata']['name']
      end
    end
  end

  describe 'E2E External transaction' do
    it 'Submit tx via POST /proxy/transactions' do
      amt = MIN_UTXO_VALUE_PURE_ADA
      address = SHELLEY.addresses.list(@target_id)[0]['id']
      target_before = get_shelley_balances(@target_id)
      src_before = get_shelley_balances(@wid)

      tx_constructed = SHELLEY.transactions.construct(@wid, payment_payload(amt, address))
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed['transaction'])
      expect(tx_decoded).to be_correct_and_respond 202

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202

      # construct tx binary blob
      serialized_tx = Base64.decode64(tx_signed['transaction'].strip)
      # submitting tx via POST /proxy/transactions
      tx_submitted = PROXY.submit_external_transaction(serialized_tx)
      expect(tx_submitted).to be_correct_and_respond 202

      # tx is visible on the src wallet as inputs belong to it
      tx_id = tx_submitted['id']
      wait_for_tx_in_ledger(@wid, tx_id)

      target_after = get_shelley_balances(@target_id)
      src_after = get_shelley_balances(@wid)
      tx = SHELLEY.transactions.get(@wid, tx_id)
      # verify actual fee the same as constructed
      expect(expected_fee).to eq tx['fee']['quantity']

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amt, expected_fee)
    end

    it 'Cannot submit unsigned tx via POST /proxy/transactions' do
      amt = MIN_UTXO_VALUE_PURE_ADA
      address = SHELLEY.addresses.list(@target_id)[0]['id']

      tx_constructed = SHELLEY.transactions.construct(@wid, payment_payload(amt, address))
      expect(tx_constructed).to be_correct_and_respond 202

      # construct tx binary blob
      serialized_tx = Base64.decode64(tx_constructed['transaction'].strip)
      # submitting tx via POST /proxy/transactions
      tx_submitted = PROXY.submit_external_transaction(serialized_tx)
      expect(tx_submitted.parsed_response).to include 'code' => 'created_invalid_transaction'
    end
  end

  describe 'E2E Migration' do
    it 'I can migrate all funds back to fixture wallet' do
      address = SHELLEY.addresses.list(@wid)[0]['id']
      src_before = get_shelley_balances(@target_id)
      target_before = get_shelley_balances(@wid)

      migration = SHELLEY.migrations.migrate(@target_id, PASS, [address])
      tx_ids = migration.map { |m| m['id'] }
      fees = migration.map { |m| m['fee']['quantity'] }.sum
      amounts = migration.map { |m| m['amount']['quantity'] }.sum - fees

      tx_ids.each do |tx_id|
        wait_for_tx_in_ledger(@target_id, tx_id)
      end
      src_after = get_shelley_balances(@target_id)
      target_after = get_shelley_balances(@wid)
      expected_src_balance = { 'total' => 0,
                               'available' => 0,
                               'rewards' => 0,
                               'assets_total' => [],
                               'assets_available' => [] }

      expect(src_after).to eq expected_src_balance

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amounts, fees)

      tx_ids.each do |tx_id|
        # examine the tx in history
        # on src wallet
        tx = SHELLEY.transactions.get(@target_id, tx_id)
        tx_inputs(tx, present: true)
        tx_outputs(tx, present: true)
        tx_direction(tx, 'outgoing')
        tx_script_validity(tx, 'valid')
        tx_status(tx, 'in_ledger')
        tx_collateral(tx, present: false)
        tx_collateral_outputs(tx, present: false)
        tx_metadata(tx, nil)
        tx_deposits(tx, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(tx, present: false)
        tx_mint_burn(tx, mint: [], burn: [])
        tx_extra_signatures(tx, present: false)
        tx_script_integrity(tx, present: false)
        tx_validity_interval_default(tx)
        tx_certificates(tx, present: false)

        # on target wallet
        txt = SHELLEY.transactions.get(@wid, tx_id)
        expect(tx['fee']['quantity'].to_i).to be > 0
        tx_inputs(txt, present: true)
        tx_outputs(txt, present: true)
        tx_direction(txt, 'incoming')
        tx_script_validity(txt, 'valid')
        tx_status(txt, 'in_ledger')
        tx_collateral(txt, present: false)
        tx_collateral_outputs(txt, present: false)
        tx_metadata(txt, nil)
        # ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
        # tx_deposits(txt, deposit_taken: 0, deposit_returned: 0)
        tx_withdrawals(txt, present: false)
        tx_mint_burn(txt, mint: [], burn: [])
        tx_extra_signatures(txt, present: false)
        tx_script_integrity(txt, present: false)
        tx_validity_interval_default(txt)
        tx_certificates(txt, present: false)
      end
    end
  end
end
