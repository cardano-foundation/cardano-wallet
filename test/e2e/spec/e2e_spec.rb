# coding: utf-8
RSpec.describe "Cardano Wallet E2E tests", :all, :e2e do

  before(:all) do
    # shelley wallets
    @wid = create_fixture_wallet(:shelley)
    @target_id = create_target_wallet(:shelley)

    # byron wallets
    @wid_rnd = create_fixture_wallet(:random)
    @wid_ic = create_fixture_wallet(:icarus)

    # shared wallets
    @wid_sha = create_target_wallet(:shared)

    @nightly_shared_wallets = [ @wid_sha ]
    @nighly_byron_wallets = [ @wid_rnd, @wid_ic ]
    @nightly_shelley_wallets = [ @wid, @target_id ]
    wait_for_all_shelley_wallets(@nightly_shelley_wallets)
    wait_for_all_shared_wallets(@nightly_shared_wallets)
    wait_for_all_byron_wallets(@nighly_byron_wallets)
  end

  after(:each) do
    teardown
  end

  after(:all) do
    SHELLEY.stake_pools.quit(@target_id, PASS)
  end

  describe "Collateral return" do
    it "AlwaysFails.plutus with collateral return to the wallet" do
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
      # export NETWORK_ID="--testnet-magic 1097911063"
      # cardano-cli address build --payment-script-file alwaysfails.plutus $NETWORK_ID > AlwaysFails.addr
      # cardano-cli transaction hash-script-data --script-data-value 1914 > datumhash
      # cardano-cli transaction build  \
      # 	--babbage-era  \
      # 	$NETWORK_ID \
      # 	--tx-in "6042918f95e9921772c0cf5c604a23c42e46cc6c6280b3bec5722041e056d2b0#0"  \
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
      else
        skip %(
                This test cannot be executed on '#{ENV['NETWORK']}' yet!
                Follow instructions in test description to prepare alwaysfails.plutus UTxO for it.
              )
      end

      # Create payment address
      payment_keys = CARDANO_CLI.generate_payment_keys
      payment_address = CARDANO_CLI.build_payment_address(payment_keys)

      # Fund payment address to be used as collateral utxo
      collateral_amt = 10000000
      payment = [{ :address => payment_address,
                   :amount => { :quantity => collateral_amt,
                                :unit => 'lovelace' }
                }]
      tx = construct_sign_submit(@wid, payment)
      wait_for_tx_in_ledger(@wid, tx.last['id'])
      collateral_utxo = CARDANO_CLI.get_utxos(payment_address).last
      # Try to spend from alwaysfails.plutus address
      target_address = SHELLEY.addresses.list(@target_id)[0]['id']
      target_before = get_shelley_balances(@target_id)
      explicit_fee = 2000000 # setting fee explicitely because we build tx raw
      txbody = CARDANO_CLI.tx_build_raw_always_fails(script = get_plutus_file_path('alwaysfails.plutus'),
                                                     script_utxo,
                                                     "#{collateral_utxo[:utxo]}##{collateral_utxo[:ix]}",
                                                     collateral_amt,
                                                     explicit_fee,
                                                     target_address,
                                                     collateral_ret_addr = target_address)
      txsigned = CARDANO_CLI.tx_sign(txbody, payment_keys)
      txid = CARDANO_CLI.tx_submit(txsigned)
      wait_for_tx_in_ledger(@target_id, txid)

      # Make sure tx properly displays collateral, collateral return and script_validity
      tx = SHELLEY.transactions.get(@target_id, txid)
      # collateral return amount to be returned is:
      #  collateral_ret_amt = collateral_amt - calculated_total_collateral_amt
      #  (calculated_total_collateral_amt = 150% * fee)
      collateral_ret_amt = collateral_amt - (explicit_fee * 1.5).to_i
      collateral = [ {"id" => collateral_utxo[:utxo], "index" => collateral_utxo[:ix].to_i} ]
      collateral_outputs = [ {"address" => target_address,
                              "amount" => {"quantity" => collateral_ret_amt, "unit" => "lovelace"},
                              "assets" => []} ]
      expect(tx['collateral']).to eq collateral
      expect(tx['collateral_outputs']).to eq collateral_outputs
      expect(tx['script_validity']).to eq 'invalid'

      # Make sure balance is correct (+collateral_ret_amt)
      target_after = get_shelley_balances(@target_id)
      expect(target_after['available']).to eq (target_before['available'] + collateral_ret_amt)

      # Make sure you can spend collateral return output from the wallet
      payment = [{ :address => payment_address,
                   :amount => { :quantity => 6500000,
                                :unit => 'lovelace' }
                }]
      tx = construct_sign_submit(@target_id, payment)
      wait_for_tx_in_ledger(@target_id, tx.last['id'])

    end
  end

  describe "E2E Balance -> Sign -> Submit" do

    def run_script(script, payload)
      tx_balanced, tx_signed, tx_submitted = balance_sign_submit(@wid, payload)
      tx_id = tx_submitted['id']

      eventually "#{script} is in ledger" do
        tx = SHELLEY.transactions.get(@wid, tx_id)
        tx.code == 200 && tx['status'] == 'in_ledger'
      end

      { tx_id: tx_id,
        tx_unbalanced: SHELLEY.transactions.decode(@wid, payload["transaction"]),
        tx_balanced: SHELLEY.transactions.decode(@wid, tx_balanced["transaction"]),
        tx_signed: SHELLEY.transactions.decode(@wid, tx_signed["transaction"]) }
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

    it "cannot balance on empty wallet" do
      wid = create_shelley_wallet
      payload = get_plutus_tx "ping-pong_1.json"
      tx_balanced = SHELLEY.transactions.balance(wid, payload)
      expect(tx_balanced).to be_correct_and_respond 403
      expect(tx_balanced.to_s).to include "not_enough_money"
    end

    it "ping-pong" do
      init_src = get_shelley_balances(@wid)
      contract_setup = "ping-pong_1.json"
      script = "ping-pong_2.json"

      # run contract setup
      payload = get_plutus_tx(contract_setup)
      r = run_script(contract_setup, payload)
      # verify that decoded balanced tx is the same as signed tx
      expect(r[:tx_balanced].parsed_response).to eq r[:tx_signed].parsed_response

      # verify wallet balance decreases as expected after transaction (by fee + amt)
      fee = r[:tx_balanced]["fee"]["quantity"]
      amt = get_sent_amt(r[:tx_balanced]["outputs"])
      src_after = get_shelley_balances(@wid)
      expect(src_after['total']).to eq (init_src['total'] - fee - amt)

      # run ping-pong_2
      src_before2 = get_shelley_balances(@wid)
      payload2 = get_templated_plutus_tx(script, { transactionId: r[:tx_id] })
      r2 = run_script(script, payload2)

      # verify that decoded balanced tx is the same as signed tx
      expect(r2[:tx_balanced].parsed_response).to eq r2[:tx_signed].parsed_response
      fee2 = r2[:tx_balanced]["fee"]["quantity"]

      # verify balance decreases as expected after transaction
      # ping-pong_2 spends from external utxo, so wallet balance decreases only by fee2
      src_after2 = get_shelley_balances(@wid)
      expect(src_after2['total']).to eq (src_before2['total'] - fee2)
    end

    it "game" do
      contract_setup = "game_1.json"
      scripts = [ "game_2.json", "game_3.json" ]

      run_contract(contract_setup, scripts)
    end

    it "mint-burn" do
      vk = SHELLEY.keys.get_public_key(@wid, 'utxo_external', 0, { hash: true }).gsub("\"", '')
      vkHash = bech32_to_base16(vk)
      policy = read_mustached_file("mintBurn_policy", { vkHash: vkHash })
      policy_id = get_policy_id(policy)
      def fingerprint
        if is_linux?
          "asset1q78ea9ds0rc3tfwu2damsjehjup2xuzddtg6xh"
        elsif is_mac?
          "asset1kjxaamf0p2p2z9g3k4xu0ne0g6h5j70st6z4pz"
        elsif is_win?
          "asset1arj5nz8zxjuxvut5wqt5q0xw7905hllugahvu7"
        end
      end
      mint_script = "mintBurn_1.json"
      burn_script = "mintBurn_2.json"
      assets = [ {"policy_script" => {"language_version" => "v1", "script_type" => "plutus"},
                  "policy_id" => policy_id,
                  "assets" => [ {"fingerprint" => fingerprint,
                                 "quantity" => 1,
                                 "asset_name" => asset_name("mint-burn") } ]
                  }
                ]

      payload_mint = get_templated_plutus_tx(mint_script, { vkHash: vkHash,
                                                          policyId: policy_id,
                                                          policy: policy })

      payload_burn = get_templated_plutus_tx(burn_script, { vkHash: vkHash,
                                                          policyId: policy_id,
                                                          policy: policy })
      mint = run_script(mint_script, payload_mint)
      burn = run_script(burn_script, payload_burn)

      # verify that decoded balanced tx is the same as signed tx
      expect(mint[:tx_balanced].parsed_response).to eq mint[:tx_signed].parsed_response
      expect(burn[:tx_balanced].parsed_response).to eq burn[:tx_signed].parsed_response

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
    end

    it "withdrawal" do
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
      # $ cardano-cli query utxo --address $(cat payment.addr) --testnet-magic 1097911063
      # $ cardano-cli transaction build  \
      # 	--alonzo-era  \
      # 	--testnet-magic 1097911063 \
      # 	--change-address "addr_test1qrfqc909vvxfq7903kaz09cuh5q2un8zw7j9ys4uh3k7j3qpgncz6fapajjvkyqka2sldfpk250nml40sf67am68wd2shl9fth" \
      # 	--tx-in "8e9dd939a6096ce0d033a8a1ad61a83f0b7188f22516c45e1a69ff8cd4ad6f4f#0"  \
      # 	--certificate-file stake.cert \
      # 	--protocol-params-file protocol.json  \
      # 	--out-file body.tx
      #
      # $ cardano-cli transaction sign \
      #    --tx-body-file body.tx \
      #    --testnet-magic 1097911063 \
      #    --signing-key-file payment.skey \
      #    --out-file signed.tx
      #
      # $ cardano-cli transaction submit --tx-file signed.tx --testnet-magic 1097911063
      validator = read_mustached_file("withdrawal_validator")
      validator_hash = get_policy_id(validator)
      withdrawal_script = "withdrawal.json"
      payload = get_templated_plutus_tx(withdrawal_script, { script: validator,
                                                            scriptHash: validator_hash })

      init_src = get_shelley_balances(@wid)

      r = run_script(withdrawal_script, payload)

      # verify wallet balance decreases as expected by fee
      fee = r[:tx_balanced]["fee"]["quantity"]
      src_after = get_shelley_balances(@wid)
      expect(src_after['total']).to eq (init_src['total'] - fee)
    end

    it "currency" do
      currency_script = "currency.json"
      currency_policy = "currency_policy"

      # Perform coin selection to select input to be used in minting contract
      address = SHELLEY.addresses.list(@wid)[0]['id']
      payload_cs = [{ :address => address,
                      :amount => { :quantity => 1000000000, :unit => "lovelace" } }
                   ]
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
      apfel = { "policy_id" => policy_id,
                "asset_name" => asset_name("apfel"),
                "quantity" => 1000 }
      banana = { "policy_id" => policy_id,
                 "asset_name" => asset_name("banana"),
                 "quantity" => 1 }

      # verify decoded transactions show that currency will be minted
      expect(r[:tx_unbalanced]['mint']['tokens'].to_s).to include policy_id
      expect(r[:tx_unbalanced]['mint']['tokens'].to_s).to include asset_name("apfel")
      expect(r[:tx_unbalanced]['mint']['tokens'].to_s).to include asset_name("banana")
      expect(r[:tx_balanced]['mint']['tokens'].to_s).to include policy_id
      expect(r[:tx_balanced]['mint']['tokens'].to_s).to include asset_name("apfel")
      expect(r[:tx_balanced]['mint']['tokens'].to_s).to include asset_name("banana")

      # make sure currency is minted as expected
      src_balance = get_shelley_balances(@wid)
      expect(src_balance['assets_total']).to include(apfel)
      expect(src_balance['assets_total']).to include(banana)

      # send out minted currency to special address not to litter fixture wallet
      payment = [{ :address => "addr_test1qqkgrywfhejgd67twkzqmx84rsr3v374pzujd5rlm0e8exnlxjupjgrqwk5dk9tard6zfwwjq4lc89szs2w599js35tqmaykuj",
                   :amount => { :quantity => 0, :unit => 'lovelace' },
                   :assets => [apfel, banana]
               }]
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

  describe "E2E Construct -> Sign -> Submit" do
    it "Single output transaction" do
      amt = 1000000
      address = SHELLEY.addresses.list(@target_id)[0]['id']
      target_before = get_shelley_balances(@target_id)
      src_before = get_shelley_balances(@wid)

      payment = [{ :address => address,
                 :amount => { :quantity => amt,
                           :unit => 'lovelace' }
               }]
      tx_constructed = SHELLEY.transactions.construct(@wid, payment)
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
      expect(tx_decoded).to be_correct_and_respond 202

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
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

    it "Multi output transaction" do
      amt = 1000000
      address = SHELLEY.addresses.list(@target_id)[0]['id']
      target_before = get_shelley_balances(@target_id)
      src_before = get_shelley_balances(@wid)

      payment = [{ :address => address,
                 :amount => { :quantity => amt,
                           :unit => 'lovelace' }
                },
                { :address => address,
                 :amount => { :quantity => amt,
                             :unit => 'lovelace' }
                }
                ]
      tx_constructed = SHELLEY.transactions.construct(@wid, payment)
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
      expect(tx_decoded).to be_correct_and_respond 202

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      target_after = get_shelley_balances(@target_id)
      src_after = get_shelley_balances(@wid)
      tx = SHELLEY.transactions.get(@wid, tx_id)
      # verify actual fee the same as constructed
      expect(expected_fee).to eq tx['fee']['quantity']

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         (2 * amt), expected_fee)
    end

    it "Multi-assets transaction" do
      amt = 1
      amt_ada = 1600000
      address = SHELLEY.addresses.list(@target_id)[1]['id']
      target_before = get_shelley_balances(@target_id)
      src_before = get_shelley_balances(@wid)

      payment = [{ "address" => address,
                  "amount" => { "quantity" => amt_ada, "unit" => "lovelace" },
                  "assets" => [ { "policy_id" => ASSETS[0]["policy_id"],
                                  "asset_name" => ASSETS[0]["asset_name"],
                                  "quantity" => amt
                                },
                                { "policy_id" => ASSETS[1]["policy_id"],
                                  "asset_name" => ASSETS[1]["asset_name"],
                                  "quantity" => amt
                                }
                              ]
                  }
                 ]

      tx_constructed = SHELLEY.transactions.construct(@wid, payment)
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
      expect(tx_decoded).to be_correct_and_respond 202

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      target_after = get_shelley_balances(@target_id)
      src_after = get_shelley_balances(@wid)
      tx = SHELLEY.transactions.get(@wid, tx_id)
      # verify actual fee the same as constructed
      expect(expected_fee).to eq tx['fee']['quantity']

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amt_ada, expected_fee)

      verify_asset_balance(src_after, src_before,
                           target_after, target_before,
                           amt)

      # Target wallet lists my associated assets
      assets = SHELLEY.assets.get(@target_id)
      expect(assets).to be_correct_and_respond 200
      expect(assets.to_s).to include ASSETS[0]["policy_id"]
      expect(assets.to_s).to include ASSETS[0]["asset_name"]
      expect(assets.to_s).to include ASSETS[0]["metadata"]["name"]
      expect(assets.to_s).to include ASSETS[1]["policy_id"]
      expect(assets.to_s).to include ASSETS[1]["asset_name"]
      expect(assets.to_s).to include ASSETS[1]["metadata"]["name"]
    end

    it "Only withdrawal" do

      balance = get_shelley_balances(@wid)
      tx_constructed = SHELLEY.transactions.construct(@wid,
                                                      payments = nil,
                                                      withdrawal = 'self')
      expect(tx_constructed).to be_correct_and_respond 202
      withdrawal = tx_constructed['coin_selection']['withdrawals'].map { |x| x['amount']['quantity'] }.first
      expect(withdrawal).to eq 0
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
      expect(tx_decoded).to be_correct_and_respond 202

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      new_balance = get_shelley_balances(@wid)
      tx = SHELLEY.transactions.get(@wid, tx_id)

      # verify actual fee the same as constructed
      expect(expected_fee).to eq tx['fee']['quantity']

      # verify balance is as expected
      expect(new_balance['available']).to eq (balance['available'] - expected_fee)
      expect(new_balance['total']).to eq (balance['total'] - expected_fee)
    end

    it "Only metadata" do
      metadata = METADATA
      balance = get_shelley_balances(@wid)
      tx_constructed = SHELLEY.transactions.construct(@wid,
                                                      payments = nil,
                                                      withdrawal = nil,
                                                      metadata)
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
      expect(tx_decoded).to be_correct_and_respond 202

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
      tx_id = tx_submitted['id']

      wait_for_tx_in_ledger(@wid, tx_id)

      new_balance = get_shelley_balances(@wid)
      tx = SHELLEY.transactions.get(@wid, tx_id)
      # verify actual fee the same as constructed
      expect(expected_fee).to eq tx['fee']['quantity']

      # verify tx has metadata
      expect(tx['metadata']).to eq metadata

      # verify balance is as expected
      expect(new_balance['available']).to eq (balance['available'] - expected_fee)
      expect(new_balance['total']).to eq (balance['total'] - expected_fee)
    end

    it "Delegation (join and quit)" do
      balance = get_shelley_balances(@target_id)
      expected_deposit = get_key_deposit
      puts "Expected deposit #{expected_deposit}"
      # Check wallet stake keys before joing stake pool
      stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
      expect(stake_keys).to be_correct_and_respond 200
      expect(stake_keys['foreign'].size).to eq 0
      expect(stake_keys['ours'].size).to eq 1
      expect(stake_keys['ours'].first['stake']['quantity']).to eq balance['total']
      expect(stake_keys['none']['stake']['quantity']).to eq 0
      expect(stake_keys['ours'].first['delegation']['active']['status']).to eq "not_delegating"

      # Pick up pool id to join
      pools = SHELLEY.stake_pools
      pool_id = pools.list({ stake: 1000 }).sample['id']

      # Join pool
      delegation = [{
                      "join" => {
                                  "pool" => pool_id,
                                  "stake_key_index" => "0H"
                                }
                    }]
      tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@target_id,
                                                                      payments = nil,
                                                                      withdrawal = nil,
                                                                      metadata = nil,
                                                                      delegation)
      # Check fee and deposit on joining
      decoded_tx = SHELLEY.transactions.decode(@target_id, tx_constructed["transaction"])
      deposit_taken = tx_constructed['coin_selection']['deposits_taken'].first['quantity']
      decoded_deposit_taken = decoded_tx['deposits_taken'].first['quantity']
      expect(deposit_taken).to eq decoded_deposit_taken
      expect(deposit_taken).to eq expected_deposit
      expect(decoded_tx['deposits_returned']).to eq []

      expected_fee = tx_constructed['fee']['quantity']
      decoded_fee = decoded_tx['fee']['quantity']
      expect(decoded_fee).to eq expected_fee

      tx_id = tx_submitted['id']
      wait_for_tx_in_ledger(@target_id, tx_id)

      # Check fee and balance and deposit after joining
      join_balance = get_shelley_balances(@target_id)
      tx = SHELLEY.transactions.get(@target_id, tx_id)
      expect(tx['fee']['quantity']).to eq expected_fee
      # expect(tx['deposit_taken']['quantity']).to eq deposit_taken
      # expect(tx['deposit_returned']['quantity']).to eq 0
      expected_join_balance = balance['total'] - deposit_taken - expected_fee
      expect(join_balance['total']).to eq expected_join_balance

      # Check wallet stake keys after joing stake pool
      stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
      expect(stake_keys).to be_correct_and_respond 200
      expect(stake_keys['foreign'].size).to eq 0
      expect(stake_keys['ours'].size).to eq 1
      expect(stake_keys['ours'].first['stake']['quantity']).to eq expected_join_balance
      expect(stake_keys['none']['stake']['quantity']).to eq 0
      expect(stake_keys['ours'].first['delegation']['active']['status']).to eq "not_delegating"
      expect(stake_keys['ours'].first['delegation']['next'].last['status']).to eq "delegating"

      # Quit pool
      quit_pool = [{ "quit" => { "stake_key_index" => "0H" } }]
      tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@target_id,
                                                                      payments = nil,
                                                                      withdrawal = nil,
                                                                      metadata = nil,
                                                                      quit_pool)

      # Check fee and deposit on quitting
      decoded_tx = SHELLEY.transactions.decode(@target_id, tx_constructed["transaction"])
      expect(decoded_tx).to be_correct_and_respond 202

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
      # tx is changed to 'incoming' and fee = 0 because deposit was returned
      expect(tx['fee']['quantity']).to eq 0
      expect(tx['direction']).to eq 'incoming'
      # expect(tx['deposit_taken']['quantity']).to eq 0
      # expect(tx['deposit_returned']['quantity']).to eq deposit_returned
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
      expect(stake_keys['ours'].first['delegation']['active']['status']).to eq "not_delegating"
      expect(stake_keys['ours'].first['delegation']['next'].first['status']).to eq "not_delegating"
      expect(stake_keys['ours'].first['delegation']['next'].last['status']).to eq "not_delegating"
    end

    describe "Minting and Burning" do
      def mint(asset_name, quantity, policy_script, address = nil)
        mint = { 'operation' => { 'mint' => { 'quantity' => quantity } },
                 'policy_script_template' => policy_script
               }
         mint['operation']['mint']['receiving_address'] = address unless address == nil
         mint['asset_name'] = asset_name unless asset_name == nil
         mint
      end

      def burn(asset_name, quantity, policy_script)
        burn = { 'operation' => { 'burn' => { 'quantity' => quantity } },
                 'policy_script_template' => policy_script
               }
        burn['asset_name'] = asset_name unless asset_name == nil
        burn
      end

      ##
      # Gets assets list in the form of 'policy_id + asset_name' array
      # @return [Array] - ["#{policy_id}#{asset_name}"...] of all minted/burnt assets
      def get_assets_from_decode(tx_decoded_mint_or_burn)
        tx_decoded_mint_or_burn['tokens'].map do |x|
           assets = x['assets'].map {|z| z['asset_name']}
           assets.map {|a| "#{x['policy_id']}#{a}"}
        end.flatten
      end

      def get_policy_id_from_decode(tx_decoded_mint_or_burn)
        tx_decoded_mint_or_burn['tokens'].first['policy_id']
      end

      ##
      # Tx1: Mints 3 x 1000 assets, each guarded by different policy script
      # Tx2: Burns 3 x 500 of each and verifies 500 of each remain on wallet
      # Tx3: Burns remaining 3 x 500 and verifies they're no longer on balance
      it "Can mint and then burn" do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script1 = 'cosigner#0'
        policy_script2 = { "all" => [ "cosigner#0" ] }
        policy_script3 = { "any" => [ "cosigner#0" ] }

        # Minting:
        mint = [mint(asset_name('Token1'), 1000, policy_script1, address),
                mint(asset_name('Token2'), 1000, policy_script2),
                mint('', 1000, policy_script3)
               ]
        create_policy_key_if_not_exists(@wid)
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting['available']).to eq (src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq (src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.map{|z| {z => 1000}}.to_set)

        # Burn half:
        burn = [burn(asset_name('Token1'), 500, policy_script1),
                burn(asset_name('Token2'), 500, policy_script2),
                burn('', 500, policy_script3)
               ]
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning['available']).to eq (src_after_minting['available'] - expected_fee)
        expect(src_after_burning['total']).to eq (src_after_minting['total'] - expected_fee)

        # verify half of assets have ben burned
        assets = assets_balance(src_after_burning['assets_total'],
                               { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.map{|z| {z => 500}}.to_set)

        # Burn all the rest:
        burn = [burn(asset_name('Token1'), 500, policy_script1),
                burn(asset_name('Token2'), 500, policy_script2),
                burn(nil, 500, policy_script3)
               ]
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning_all = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning_all['available']).to eq (src_after_burning['available'] - expected_fee)
        expect(src_after_burning_all['total']).to eq (src_after_burning['total'] - expected_fee)

        # verify all assets have been burned and no longer on wallet's balance
        assets = assets_balance(src_after_burning_all['assets_total'],
                               { assets_to_check: assets_to_check })
        expect(assets).to eq({}.to_set)
      end

      ##
      # Tx1: Mints 3 x 1 assets with metadata
      # Tx2: Burns 3 x 1 assets and also assign metadata to tx
      it "Can mint and burn with metadata" do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script1 = 'cosigner#0'
        policy_script2 = 'cosigner#0'
        policy_script3 = { "any" => [ "cosigner#0" ] }
        metadata = METADATA
        assets_quantity = 1

        # Minting:
        mint = [mint(asset_name('TokenMetadata1'), assets_quantity, policy_script1, address),
                mint(asset_name('TokenMetadata2'), assets_quantity, policy_script2),
                mint(asset_name('TokenMetadata3'), assets_quantity, policy_script3)
               ]
        create_policy_key_if_not_exists(@wid)
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata,
                                                                        delegations = nil,
                                                                        mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
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
        expect(src_after_minting['available']).to eq (src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq (src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.map{|z| {z => assets_quantity}}.to_set)

        # Burn all:
        burn = [burn(asset_name('TokenMetadata1'), assets_quantity, policy_script1),
                burn(asset_name('TokenMetadata2'), assets_quantity, policy_script2),
                burn(asset_name('TokenMetadata3'), assets_quantity, policy_script3)
               ]
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata,
                                                                        delegations = nil,
                                                                        burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
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
        expect(src_after_burning['available']).to eq (src_after_minting['available'] - expected_fee)
        expect(src_after_burning['total']).to eq (src_after_minting['total'] - expected_fee)

        # verify all assets have been burned and no longer on wallet's balance
        assets = assets_balance(src_after_burning['assets_total'],
                               { assets_to_check: assets_to_check })
        expect(assets).to eq({}.to_set)
      end

      ##
      # Mint NFT with CIP-25 metadata
      it "Can mint NFT attaching CIP-25 metadata" do
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
        cip25_metadata = { "721" => {
                              "#{policy_id}" => {
                                    "#{nft_name}" => {
                                        "name" => "NFT FTW: #{nft_name}",
                                        "image" => "ipfs://XXXXYYYYZZZZ"
                                      }
                                    }
                                  }
                          }

        # Minting:
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        cip25_metadata,
                                                                        delegations = nil,
                                                                        mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting = get_shelley_balances(@wid)

        # verify tx has metadata
        tx = SHELLEY.transactions.get(@wid, tx_id, "simple-metadata" => true)
        expect(tx['metadata']).to eq cip25_metadata

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting['available']).to eq (src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq (src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.map{|z| {z => assets_quantity}}.to_set)

        # Burn:
        burn = [burn(nft_name_hex, assets_quantity, policy_script)]

        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning['available']).to eq (src_after_minting['available'] - expected_fee)
        expect(src_after_burning['total']).to eq (src_after_minting['total'] - expected_fee)

        # verify all assets have been burned and no longer on wallet's balance
        assets = assets_balance(src_after_burning['assets_total'],
                               { assets_to_check: assets_to_check })
        expect(assets).to eq({}.to_set)
      end

      ##
      # Tx1: Mints 2 x 500 assets, each guarded by different policy script => A1 = 500, A2 = 500
      # Tx2: Mints 500 more of A1 and burns 500 of A2 => A1 = 1000, A2 = 0
      # Tx3: Burns remaining 1000 of A1 => A1 = 0, A2 = 0
      it "Can mint and burn in the same tx" do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script1 = { "some" => {"at_least" => 1, "from" => [ "cosigner#0" ]} }
        policy_script2 = { "any" => [ "cosigner#0" ] }

        # Minting:
        mint = [mint(asset_name('Asset1'), 500, policy_script1, address),
                mint(asset_name('Asset2'), 500, policy_script2)]

        create_policy_key_if_not_exists(@wid)
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting['available']).to eq (src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq (src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.map{|z| {z => 500}}.to_set)

        # Minting and burning:
        mint_burn = [mint(asset_name('Asset1'), 500, policy_script1),
                     burn(asset_name('Asset2'), 500, policy_script2)]

        # p JSON.parse(mint_burn.to_json)
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        mint_burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting_burning['available']).to eq (src_after_minting['available'] - expected_fee)
        expect(src_after_minting_burning['total']).to eq (src_after_minting['total'] - expected_fee)

        # verify Asset1 has been minted and Asset2 burned
        assets_minted_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets_burned_to_check = get_assets_from_decode(tx_decoded['burn'])
        assets_minted = assets_balance(src_after_minting_burning['assets_total'],
                                       { assets_to_check: assets_minted_to_check })
        assets_burned = assets_balance(src_after_minting_burning['assets_total'],
                                       { assets_to_check: assets_burned_to_check })

        expect(assets_minted).to eq(assets_minted_to_check.map{|z| {z => 1000}}.to_set)
        expect(assets_burned).to eq({}.to_set)

        # Burn all the rest:
        burn = [burn(asset_name('Asset1'), 1000, policy_script1)]
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        burn)

        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning['available']).to eq (src_after_minting_burning['available'] - expected_fee)
        expect(src_after_burning['total']).to eq (src_after_minting_burning['total'] - expected_fee)

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
      it "Can mint and burn the same asset in single tx" do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script = "cosigner#0"
        assets_name = asset_name('MintBurnX')

        # Minting 10 MintBurn:
        mint = [mint(assets_name, 10, policy_script, address)]

        create_policy_key_if_not_exists(@wid)
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        mint)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting['available']).to eq (src_before['available'] - expected_fee)
        expect(src_after_minting['total']).to eq (src_before['total'] - expected_fee)

        # verify assets have been minted and on wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(src_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.map{|z| {z => 10}}.to_set)

        # Burning 10 MintBurn and minting 1 MintBurn:
        mint_burn = [burn(assets_name, 10, policy_script),
                     mint(assets_name, 1, policy_script, address)]

        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        mint_burn)
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_minting_burning['available']).to eq (src_after_minting['available'] - expected_fee)
        expect(src_after_minting_burning['total']).to eq (src_after_minting['total'] - expected_fee)

        # verify MintBurn has 1 (because 10 was burned and 1 additional minted)
        assets_minted_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets_burned_to_check = get_assets_from_decode(tx_decoded['burn'])
        assets_minted = assets_balance(src_after_minting_burning['assets_total'],
                                       { assets_to_check: assets_minted_to_check })
        assets_burned = assets_balance(src_after_minting_burning['assets_total'],
                                       { assets_to_check: assets_burned_to_check })

        expect(assets_minted).to eq(assets_minted_to_check.map{|z| {z => 1}}.to_set)

        # Burn all the rest:
        burn = [burn(assets_name, 1, policy_script)]
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        burn)

        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_burning = get_shelley_balances(@wid)

        # verify ADA balance is correct (fee is deducted)
        expect(src_after_burning['available']).to eq (src_after_minting_burning['available'] - expected_fee)
        expect(src_after_burning['total']).to eq (src_after_minting_burning['total'] - expected_fee)

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
                                                        payments = nil,
                                                        withdrawal = nil,
                                                        metadata = nil,
                                                        delegations = nil,
                                                        burn)
        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code'].to_s).to eq "not_enough_money"
        expect(tx_constructed['message'].to_s).to include "token: #{asset_name('AmazingNFTIdontHave')}"
        expect(tx_constructed['message'].to_s).to include "quantity: 1"
      end

      ##
      # Tx1: Mints 1000 assets
      # Tx2: Fails to burn 1000 assets using different policy_script
      # Tx2: Fails to burn 1022 assets using correct policy_script
      # Tx3: Burns remaining 1000 assets
      it "Cannot burn with wrong policy_script or more than I have" do
        src_before = get_shelley_balances(@wid)
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script = 'cosigner#0'

        # Mint it:
        mint = [mint(asset_name('MintIt'), 1000, policy_script, address)]
        create_policy_key_if_not_exists(@wid)
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        mint)
        expected_fee = tx_constructed['fee']['quantity']

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)

        # Try to burn:
        #  - with different policy_script
        burn1 = [burn(asset_name('MintIt'), 1000, {"all" => ["cosigner#0"]})]
        tx_constructed = SHELLEY.transactions.construct(@wid,
                                                        payments = nil,
                                                        withdrawal = nil,
                                                        metadata = nil,
                                                        delegations = nil,
                                                        burn1)

        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code'].to_s).to eq "not_enough_money"
        expect(tx_constructed['message'].to_s).to include "token: #{asset_name('MintIt')}"
        expect(tx_constructed['message'].to_s).to include "quantity: 1000"

        #  - correct policy_script but too much
        burn2 = [burn(asset_name('MintIt'), 1022, policy_script)]
        tx_constructed = SHELLEY.transactions.construct(@wid,
                                                       payments = nil,
                                                       withdrawal = nil,
                                                       metadata = nil,
                                                       delegations = nil,
                                                       burn2)

        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code'].to_s).to eq "not_enough_money"
        expect(tx_constructed['message'].to_s).to include "token: #{asset_name('MintIt')}"
        expect(tx_constructed['message'].to_s).to include "quantity: 22"

        # Burn it:
        burn3 = [burn(asset_name('MintIt'), 1000, policy_script)]
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        burn3)
        expected_fee = tx_constructed['fee']['quantity']
        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])

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

        src_before = get_shelley_balances(@wid)
        target_before = get_shelley_balances(@target_id)
        address = SHELLEY.addresses.list(@target_id).first['id']
        policy_script = 'cosigner#0'
        assets_quantity = 1500
        assets_name = asset_name('ToForeignWallet')

        # Mint it:
        mint = [mint(assets_name, assets_quantity, policy_script, address)]
        create_policy_key_if_not_exists(@wid)
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        mint)

        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid, tx_id)
        src_after_minting = get_shelley_balances(@wid)
        target_after_minting = get_shelley_balances(@target_id)

        # verify ADA balance is correct on src wallet:
        # we are minting and sending to external address
        # therefore the cost is fee + mintUTxOValue of ADA that is required
        # for transfering the assets over the network
        # in this the total `cost` is pure ADA minUTxOValue + 11 'utxo words' + fee
        min_utxo_value = NETWORK.parameters['minimum_utxo_value']['quantity'].to_i
        era = NETWORK.information['node_era']
        lovelace_per_utxo_word = (era == 'babbage' ? 34480 : 34482)
        min_utxo_value_tokens = min_utxo_value + 11 * lovelace_per_utxo_word
        expect(src_after_minting['available']).to eq (src_before['available'] - expected_fee - min_utxo_value_tokens)
        expect(src_after_minting['total']).to eq (src_before['total'] - expected_fee - min_utxo_value_tokens)

        # verify assets have been minted and on target wallet's balance
        assets_to_check = get_assets_from_decode(tx_decoded['mint'])
        assets = assets_balance(target_after_minting['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.map{|z| {z => assets_quantity}}.to_set)

        # Try to burn on target wallet and fail:
        create_policy_key_if_not_exists(@target_id)
        burn = [burn(assets_name, assets_quantity, policy_script)]
        tx_constructed = SHELLEY.transactions.construct(@target_id,
                                                        payments = nil,
                                                        withdrawal = nil,
                                                        metadata = nil,
                                                        delegations = nil,
                                                        burn)
        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code'].to_s).to eq "not_enough_money"
        expect(tx_constructed['message'].to_s).to include "token: #{assets_name}"
        expect(tx_constructed['message'].to_s).to include "quantity: #{assets_quantity}"

        # Send them back to src wallet:
        src_address = SHELLEY.addresses.list(@wid).first['id']
        policy_id = get_policy_id_from_decode(tx_decoded['mint'])
        # Make sure decoded policy id correct
        expect(policy_id).to eq SHELLEY.keys.create_policy_id(@wid, policy_script)['policy_id']
        payment = [{ "address" => src_address,
                    "amount" => { "quantity" => 0, "unit" => "lovelace" },
                    "assets" => [ { "policy_id" => policy_id,
                                    "asset_name" => assets_name,
                                    "quantity" => assets_quantity
                                  } ]
                    }
                   ]

        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@target_id, payment)
        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@target_id, tx_id)
        src_after_sending = get_shelley_balances(@wid)
        assets = assets_balance(src_after_sending['assets_total'], { assets_to_check: assets_to_check })
        expect(assets).to eq(assets_to_check.map{|z| {z => assets_quantity}}.to_set)

        # Burn them on src wallet:
        burn = [burn(assets_name, assets_quantity, policy_script)]
        tx_constructed, tx_signed, tx_submitted = construct_sign_submit(@wid,
                                                                        payments = nil,
                                                                        withdrawal = nil,
                                                                        metadata = nil,
                                                                        delegations = nil,
                                                                        burn)

        tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
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
      describe "Mint/Burn quantities" do
        matrix = [
                  [-9223372036854775808, 400, "bad_request"],
                  [-1, 400, "bad_request"],
                  [0, 403, "mint_or_burn_asset_quantity_out_of_bounds"],
                  [9223372036854775808, 403, "mint_or_burn_asset_quantity_out_of_bounds"]
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
                                                            payments = nil,
                                                            withdrawal = nil,
                                                            metadata = nil,
                                                            delegations = nil,
                                                            mint)
            expect(tx_constructed).to be_correct_and_respond code
            expect(tx_constructed['code'].to_s).to eq message
            # expect(tx_constructed['message'].to_s).to include "token: #{asset_name('AmazingNFTIdontHave')}"
            # expect(tx_constructed['message'].to_s).to include "quantity: 1"
          end

          it "Cannot burn #{quantity} assets" do
            policy_script = 'cosigner#0'
            assets_name = asset_name('AmazingNFTIdontHave')
            assets_quantity = quantity
            burn = [burn(assets_name, assets_quantity, policy_script)]
            create_policy_key_if_not_exists(@wid)
            tx_constructed = SHELLEY.transactions.construct(@wid,
                                                            payments = nil,
                                                            withdrawal = nil,
                                                            metadata = nil,
                                                            delegations = nil,
                                                            burn)
            expect(tx_constructed).to be_correct_and_respond code
            expect(tx_constructed['code'].to_s).to eq message
          end
        end
      end

      ##
      # Make sure minting above boundary asset_name values returns proper error
      describe "Mint/Burn asset_name" do
        matrix = [
                  ['too long', '1' * 66, 403, "asset_name_too_long"],
                  ['invalid hex', '1', 400, "bad_request"]
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
                                                            payments = nil,
                                                            withdrawal = nil,
                                                            metadata = nil,
                                                            delegations = nil,
                                                            mint)

            expect(tx_constructed).to be_correct_and_respond code
            expect(tx_constructed['code']).to eq message
          end
        end

      end

      ##
      # Test against some invalid policy script examples
      describe "Mint/Burn invalid policy script" do
        matrix = [
                  ['cosigner#1', 403, "created_wrong_policy_script_template"],
                  [{ "all" => [ "cosigner#0", "cosigner#1" ] }, 403, "created_wrong_policy_script_template"],
                  [{ "any" => [ "cosigner#0", "cosigner#0" ] }, 403, "created_wrong_policy_script_template"],
                  [{ "some" => {"at_least" => 2, "from" => [ "cosigner#0" ]}}, 403, "created_wrong_policy_script_template"],
                  [{ "some" => [ "cosigner#0" ] }, 400, "bad_request"]
                 ]

        matrix.each do |m|
          policy_script = m[0]
          code = m[1]
          message = m[2]
          it "Cannot mint if my policy script is invalid: #{policy_script}" do
            address = SHELLEY.addresses.list(@wid).first['id']
            assets_name = asset_name("WillNotMintIt")
            assets_quantity = 1500
            mint = [mint(assets_name, assets_quantity, policy_script, address)]
            tx_constructed = SHELLEY.transactions.construct(@wid,
                                                            payments = nil,
                                                            withdrawal = nil,
                                                            metadata = nil,
                                                            delegations = nil,
                                                            mint)

            expect(tx_constructed).to be_correct_and_respond code
            expect(tx_constructed['code']).to eq message
          end
        end

      end

      it "Cannot mint if I make too big transaction" do
        address = SHELLEY.addresses.list(@wid).first['id']
        policy_script = 'cosigner#0'
        assets_quantity = 1500

        mint = []
        1000.times do |i|
          mint << mint(asset_name("TooBIG#{i}"), assets_quantity, policy_script, address)
        end
        tx_constructed = SHELLEY.transactions.construct(@wid,
                                                        payments = nil,
                                                        withdrawal = nil,
                                                        metadata = nil,
                                                        delegations = nil,
                                                        mint)

        expect(tx_constructed).to be_correct_and_respond 403
        expect(tx_constructed['code']).to eq 'output_token_bundle_size_exceeds_limit'
      end
    end
  end

  describe "E2E Shared" do
    it "I can receive transaction to shared wallet" do
      amt = 1
      amt_ada = 3000000
      address = SHARED.addresses.list(@wid_sha)[1]['id']
      target_before = get_shared_balances(@wid_sha)
      src_before = get_shelley_balances(@wid)

      payload = [{ "address" => address,
                  "amount" => { "quantity" => amt_ada, "unit" => "lovelace" },
                  "assets" => [ { "policy_id" => ASSETS[0]["policy_id"],
                                  "asset_name" => ASSETS[0]["asset_name"],
                                  "quantity" => amt
                                },
                                { "policy_id" => ASSETS[1]["policy_id"],
                                  "asset_name" => ASSETS[1]["asset_name"],
                                  "quantity" => amt
                                }
                              ]
                  }
                 ]

      tx_sent = SHELLEY.transactions.create(@wid, PASS, payload)

      expect(tx_sent).to be_correct_and_respond 202
      expect(tx_sent.to_s).to include "pending"
      wait_for_tx_in_ledger(@wid, tx_sent['id'])

      target_after = get_shared_balances(@wid_sha)
      src_after = get_shelley_balances(@wid)
      fee = SHELLEY.transactions.get(@wid, tx_sent['id'])['fee']['quantity']

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amt_ada, fee)

      verify_asset_balance(src_after, src_before,
                           target_after, target_before,
                           amt)
    end
  end

  describe "E2E Shelley" do
    describe "Native Assets" do
      it "I can list native assets" do
        assets = SHELLEY.assets.get @wid
        expect(assets).to be_correct_and_respond 200
        expect(assets.to_s).to include ASSETS[0]["policy_id"]
        expect(assets.to_s).to include ASSETS[0]["asset_name"]
        expect(assets.to_s).to include ASSETS[1]["policy_id"]
        expect(assets.to_s).to include ASSETS[1]["asset_name"]

        assets = SHELLEY.assets.get(@wid, policy_id = ASSETS[0]["policy_id"])
        expect(assets).to be_correct_and_respond 200
        expect(assets["policy_id"]).to eq ASSETS[0]["policy_id"]
        expect(assets["asset_name"]).to eq ASSETS[0]["asset_name"]
        expect(assets["asset_name"]).not_to eq ASSETS[1]["asset_name"]

        assets = SHELLEY.assets.get(@wid, policy_id = ASSETS[1]["policy_id"],
                                          asset_name = ASSETS[1]["asset_name"])
        expect(assets).to be_correct_and_respond 200
        expect(assets["policy_id"]).to eq ASSETS[1]["policy_id"]
        expect(assets["asset_name"]).to eq ASSETS[1]["asset_name"]
        expect(assets["asset_name"]).not_to eq ASSETS[0]["asset_name"]
      end

      it "I can list native assets and get offchain metadata", :offchain do
        assets = SHELLEY.assets.get @wid
        expect(assets).to be_correct_and_respond 200
        expect(assets.to_s).to include ASSETS[0]["policy_id"]
        expect(assets.to_s).to include ASSETS[0]["asset_name"]
        expect(assets.to_s).to include ASSETS[0]["metadata"]["name"]
        expect(assets.to_s).to include ASSETS[1]["policy_id"]
        expect(assets.to_s).to include ASSETS[1]["asset_name"]
        expect(assets.to_s).to include ASSETS[1]["metadata"]["name"]

        assets = SHELLEY.assets.get(@wid, policy_id = ASSETS[0]["policy_id"])
        expect(assets).to be_correct_and_respond 200
        expect(assets["policy_id"]).to eq ASSETS[0]["policy_id"]
        expect(assets["asset_name"]).to eq ASSETS[0]["asset_name"]
        expect(assets["metadata"]).to eq ASSETS[0]["metadata"]
        expect(assets["asset_name"]).not_to eq ASSETS[1]["asset_name"]
        expect(assets["metadata"]).not_to eq ASSETS[1]["metadata"]

        assets = SHELLEY.assets.get(@wid, policy_id = ASSETS[1]["policy_id"],
                                          asset_name = ASSETS[1]["asset_name"])
        expect(assets).to be_correct_and_respond 200
        expect(assets["policy_id"]).to eq ASSETS[1]["policy_id"]
        expect(assets["asset_name"]).to eq ASSETS[1]["asset_name"]
        expect(assets["metadata"]).to eq ASSETS[1]["metadata"]
        expect(assets["asset_name"]).not_to eq ASSETS[0]["asset_name"]
        expect(assets["metadata"]).not_to eq ASSETS[0]["metadata"]["name"]
      end

      it "I can send native assets tx and they are received" do
        amt = 1
        address = SHELLEY.addresses.list(@target_id)[1]['id']
        target_before = get_shelley_balances(@target_id)

        payload = [{ "address" => address,
                    "amount" => { "quantity" => 0, "unit" => "lovelace" },
                    "assets" => [ { "policy_id" => ASSETS[0]["policy_id"],
                                    "asset_name" => ASSETS[0]["asset_name"],
                                    "quantity" => amt
                                  },
                                  { "policy_id" => ASSETS[1]["policy_id"],
                                    "asset_name" => ASSETS[1]["asset_name"],
                                    "quantity" => amt
                                  }
                                ]
                    }
                   ]

        tx_sent = SHELLEY.transactions.create(@wid, PASS, payload)

        expect(tx_sent).to be_correct_and_respond 202
        expect(tx_sent.to_s).to include "pending"
        wait_for_tx_in_ledger(@wid, tx_sent['id'])


        target_after = get_shelley_balances(@target_id)

        # verify balances are correct on target wallet
        assets_to_check = ["#{ASSETS[0]["policy_id"]}#{ASSETS[0]["asset_name"]}",
                           "#{ASSETS[1]["policy_id"]}#{ASSETS[1]["asset_name"]}"]
        target_total_after = assets_balance(target_after['assets_total'], { assets_to_check: assets_to_check })
        target_avail_after = assets_balance(target_after['assets_available'], { assets_to_check: assets_to_check })
        target_total_expected = assets_balance(target_before['assets_total'], { assets_to_check: assets_to_check, delta: (+amt) })
        target_avail_expected = assets_balance(target_before['assets_available'], { assets_to_check: assets_to_check, delta: (+amt) })
        if target_before['assets_total'] == []
          target_balance_expected = assets_to_check.map { |a| { a => amt } }.to_set
          expect(target_total_after).to eq target_balance_expected
          expect(target_avail_after).to eq target_balance_expected
        else
          expect(target_total_after).to eq target_total_expected
          expect(target_avail_after).to eq target_avail_expected
        end
      end

    end

    describe "Shelley Migrations" do
      it "I can create migration plan shelley -> shelley" do
        addrs = SHELLEY.addresses.list(@target_id).map { |a| a['id'] }

        plan = SHELLEY.migrations.plan(@wid, addrs)
        expect(plan).to be_correct_and_respond 202
        expect(plan['balance_selected']['assets']).not_to be []
        expect(plan['balance_leftover']).to eq ({ "ada" => { "quantity" => 0,
                                                         "unit" => "lovelace" },
                                                 "assets" => [] })
      end
    end

    describe "Shelley Transactions" do
      it "I can send transaction and funds are received" do
        amt = 1000000

        address = SHELLEY.addresses.list(@target_id)[0]['id']
        available_before = SHELLEY.wallets.get(@target_id)['balance']['available']['quantity']
        total_before = SHELLEY.wallets.get(@target_id)['balance']['total']['quantity']

        tx_sent = SHELLEY.transactions.create(@wid, PASS, [{ address => amt }])

        expect(tx_sent).to be_correct_and_respond 202
        expect(tx_sent.to_s).to include "pending"

        eventually "Funds are on target wallet: #{@target_id}" do
          available = SHELLEY.wallets.get(@target_id)['balance']['available']['quantity']
          total = SHELLEY.wallets.get(@target_id)['balance']['total']['quantity']
          (available == amt + available_before) && (total == amt + total_before)
        end
      end

      it "I can send transaction with ttl and funds are received" do
        amt = 1000000
        ttl_in_s = 1200

        address = SHELLEY.addresses.list(@target_id)[0]['id']
        target_before = get_shelley_balances(@target_id)
        tx_sent = SHELLEY.transactions.create(@wid,
                                              PASS,
                                              [{ address => amt }],
                                              withdrawal = nil,
                                              metadata = nil,
                                              ttl_in_s)

        expect(tx_sent).to be_correct_and_respond 202
        expect(tx_sent.to_s).to include "pending"
        wait_for_tx_in_ledger(@wid, tx_sent['id'])

        target_after = get_shelley_balances(@target_id)

        expect(target_after['available']).to eq (amt + target_before['available'])
        expect(target_after['total']).to eq (amt + target_before['total'])
        expect(target_after['reward']).to eq (target_before['reward'])
      end

      it "Transaction with ttl = 0 would expire and I can forget it" do
        skip "Test is flaky due to ADP-608"
        amt = 1000000
        ttl_in_s = 0

        address = SHELLEY.addresses.list(@target_id)[0]['id']
        tx_sent = SHELLEY.transactions.create(@wid,
                                              PASS,
                                              [{ address => amt }],
                                              withdrawal = nil,
                                              metadata = nil,
                                              ttl_in_s)

        expect(tx_sent).to be_correct_and_respond 202
        expect(tx_sent.to_s).to include "pending"

        eventually "TX `#{tx_sent['id']}' expires on `#{@wid}'" do
          SHELLEY.transactions.get(@wid, tx_sent['id'])['status'] == 'expired'
        end

        res = SHELLEY.transactions.forget(@wid, tx_sent['id'])
        expect(res).to be_correct_and_respond 204

        fres = SHELLEY.transactions.get(@wid, tx_sent['id'])
        expect(fres).to be_correct_and_respond 404
      end

      it "I can send transaction using 'withdrawal' flag and funds are received" do
        amt = 1000000
        address = SHELLEY.addresses.list(@target_id)[0]['id']
        target_before = get_shelley_balances(@target_id)
        src_before = get_shelley_balances(@wid)

        tx_sent = SHELLEY.transactions.create(@wid, PASS, [{ address => amt }], 'self')

        expect(tx_sent).to be_correct_and_respond 202
        expect(tx_sent.to_s).to include "pending"
        wait_for_tx_in_ledger(@wid, tx_sent['id'])

        fee = SHELLEY.transactions.get(@wid, tx_sent['id'])['fee']['quantity']
        target_after = get_shelley_balances(@target_id)
        src_after = get_shelley_balances(@wid)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt, fee)
      end

      it "I can send transaction with metadata" do
        amt = 1000000
        metadata = METADATA

        address = SHELLEY.addresses.list(@target_id)[0]['id']
        target_before = get_shelley_balances(@target_id)

        tx_sent = SHELLEY.transactions.create(@wid,
                                              PASS,
                                              [{ address => amt }],
                                              nil,
                                              metadata
                                             )

        expect(tx_sent).to be_correct_and_respond 202
        expect(tx_sent.to_s).to include "pending"
        wait_for_tx_in_ledger(@wid, tx_sent['id'])

        target_after = get_shelley_balances(@target_id)

        expect(target_after['available']).to eq (amt + target_before['available'])
        expect(target_after['total']).to eq (amt + target_before['total'])
        expect(target_after['reward']).to eq (target_before['reward'])

        meta_src = SHELLEY.transactions.get(@wid, tx_sent['id'])['metadata']
        meta_dst = SHELLEY.transactions.get(@target_id, tx_sent['id'])['metadata']
        expect(meta_src).to eq metadata
        expect(meta_dst).to eq metadata

      end

      it "I can estimate fee" do
        metadata = METADATA

        address = SHELLEY.addresses.list(@target_id)[0]['id']
        amt = [{ address => 1000000 }]

        txs = SHELLEY.transactions
        fees = txs.payment_fees(@wid, amt)
        expect(fees).to be_correct_and_respond 202

        fees = txs.payment_fees(@wid, amt, 'self')
        expect(fees).to be_correct_and_respond 202

        fees = txs.payment_fees(@wid, amt, 'self', metadata)
        expect(fees).to be_correct_and_respond 202
      end
    end

    describe "Stake Pools" do

      it "I could join Stake Pool - if I knew it's id" do
        pools = SHELLEY.stake_pools

        join = pools.join(SPID, @wid, PASS)
        expect(join).to be_correct_and_respond 404
        expect(join.to_s).to include "no_such_pool"
      end

      it "I could check delegation fees - if I could cover fee" do
        id = create_shelley_wallet

        pools = SHELLEY.stake_pools
        fees = pools.delegation_fees(id)
        expect(fees).to be_correct_and_respond 403
        expect(fees.to_s).to include "not_enough_money"
      end

      it "I could join Stake Pool - if I had enough to cover fee" do
        id = create_shelley_wallet
        pools = SHELLEY.stake_pools
        pool_id = pools.list({ stake: 1000 })[0]['id']

        join = pools.join(pool_id, id, PASS)
        expect(join).to be_correct_and_respond 403
        expect(join.to_s).to include "not_enough_money"
      end

      it "Can list stake pools only when stake is provided" do
        pools = SHELLEY.stake_pools
        l = pools.list({ stake: 1000 })
        l_bad = pools.list
        expect(l).to be_correct_and_respond 200

        expect(l_bad).to be_correct_and_respond 400
        expect(l_bad.to_s).to include "query_param_missing"
      end

      it "Can join and quit Stake Pool" do
        # Get funds on the wallet
        address = SHELLEY.addresses.list(@target_id)[0]['id']
        amt = 10000000
        deposit = get_key_deposit
        tx_sent = SHELLEY.transactions.create(@wid,
                                              PASS,
                                              [{ address => amt }])

        expect(tx_sent).to be_correct_and_respond 202
        expect(tx_sent.to_s).to include "pending"
        wait_for_tx_in_ledger(@wid, tx_sent['id'])

        stake = get_shelley_balances(@target_id)['total']

        # Check wallet stake keys before joing stake pool
        stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
        expect(stake_keys).to be_correct_and_respond 200
        expect(stake_keys['foreign'].size).to eq 0
        expect(stake_keys['ours'].size).to eq 1
        expect(stake_keys['ours'].first['stake']['quantity']).to eq stake
        expect(stake_keys['none']['stake']['quantity']).to eq 0
        expect(stake_keys['ours'].first['delegation']['active']['status']).to eq "not_delegating"
        # expect(stake_keys['ours'].first['delegation']['next']).to eq []

        # Pick up pool id to join
        pools = SHELLEY.stake_pools
        pool_id = pools.list({ stake: 1000 }).sample['id']

        # Join pool
        puts "Joining pool: #{pool_id}"
        join = pools.join(pool_id, @target_id, PASS)

        expect(join).to be_correct_and_respond 202
        expect(join.to_s).to include "status"

        join_tx_id = join['id']
        eventually "Checking if join tx id (#{join_tx_id}) is in_ledger" do
          tx = SHELLEY.transactions.get(@target_id, join_tx_id)
          tx['status'] == "in_ledger"
        end
        fee = SHELLEY.transactions.get(@target_id, join_tx_id)['fee']['quantity']

        stake_after_joining = stake - deposit - fee

        # Check wallet stake keys after joing stake pool
        stake_keys = SHELLEY.stake_pools.list_stake_keys(@target_id)
        expect(stake_keys).to be_correct_and_respond 200
        expect(stake_keys['foreign'].size).to eq 0
        expect(stake_keys['ours'].size).to eq 1
        expect(stake_keys['ours'].first['stake']['quantity']).to eq stake_after_joining
        expect(stake_keys['none']['stake']['quantity']).to eq 0
        expect(stake_keys['ours'].first['delegation']['active']['status']).to eq "not_delegating"
        expect(stake_keys['ours'].first['delegation']['next'].last['status']).to eq "delegating"

        # Quit pool
        puts "Quitting pool: #{pool_id}"
        quit = pools.quit(@target_id, PASS)

        expect(quit).to be_correct_and_respond 202
        expect(quit.to_s).to include "status"

        quit_tx_id = quit['id']
        eventually "Checking if quit tx id (#{quit_tx_id}) is in_ledger" do
          tx = SHELLEY.transactions.get(@target_id, quit_tx_id)
          tx['status'] == "in_ledger"
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
        expect(stake_keys['ours'].first['delegation']['active']['status']).to eq "not_delegating"
        expect(stake_keys['ours'].first['delegation']['next'].first['status']).to eq "not_delegating"
        expect(stake_keys['ours'].first['delegation']['next'].last['status']).to eq "not_delegating"
      end
    end

    describe "Coin Selection" do

      it "I can trigger random coin selection" do
        addresses = SHELLEY.addresses.list(@target_id)
        payload = [{ "address" => addresses[0]['id'],
                    "amount" => { "quantity" => 2000000, "unit" => "lovelace" },
                    "assets" => [ { "policy_id" => ASSETS[0]["policy_id"],
                                    "asset_name" => ASSETS[0]["asset_name"],
                                    "quantity" => 1
                                  },
                                  { "policy_id" => ASSETS[1]["policy_id"],
                                    "asset_name" => ASSETS[1]["asset_name"],
                                    "quantity" => 1
                                  }
                                ]
                    }
                   ]

        rnd = SHELLEY.coin_selections.random(@wid, payload, withdrawal = "self", m = METADATA)

        expect(rnd).to be_correct_and_respond 200
        expect(rnd.to_s).to include "outputs"
        expect(rnd.to_s).to include "change"
        expect(rnd.to_s).to include "metadata"
        expect(rnd['inputs']).not_to be_empty
        expect(rnd['outputs']).not_to be_empty
      end

      it "I can trigger random coin selection delegation action" do
        pid = SHELLEY.stake_pools.list({ stake: 1000000 }).sample['id']
        action_join = { action: "join", pool: pid }

        rnd = SHELLEY.coin_selections.random_deleg @wid, action_join

        expect(rnd).to be_correct_and_respond 200
        expect(rnd.to_s).to include "outputs"
        expect(rnd.to_s).to include "change"
        expect(rnd['inputs']).not_to be_empty
        expect(rnd['change']).not_to be_empty
        expect(rnd['outputs']).to be_empty
        expect(rnd['certificates']).not_to be_empty
        # expect(rnd['certificates'].to_s).to include "register_reward_account"
        expect(rnd['certificates'].to_s).to include "join_pool"
      end

      it "I could trigger random coin selection delegation action - if I had money" do
        wid = create_shelley_wallet
        pid = SHELLEY.stake_pools.list({ stake: 1000000 }).sample['id']
        action_join = { action: "join", pool: pid }

        rnd = SHELLEY.coin_selections.random_deleg wid, action_join
        expect(rnd).to be_correct_and_respond 403
        expect(rnd.to_s).to include "not_enough_money"
      end

      it "I could trigger random coin selection delegation action - if I known pool id" do
        addresses = SHELLEY.addresses.list(@wid)
        action_join = { action: "join", pool: SPID_BECH32 }
        action_quit = { action: "quit" }

        rnd = SHELLEY.coin_selections.random_deleg @wid, action_join
        expect(rnd).to be_correct_and_respond 404
        expect(rnd.to_s).to include "no_such_pool"

        rnd = SHELLEY.coin_selections.random_deleg @wid, action_quit
        expect(rnd).to be_correct_and_respond 403
        expect(rnd.to_s).to include "not_delegating_to"
      end

    end

    describe "Update passphrase" do
      it "I can update passphrase with mnemonic and the wallet does not have to re-sync" do
        mnemonics = get_fixture_wallet_mnemonics(:fixture, :shelley)
        upd = SHELLEY.wallets.update_passphrase(@wid, { mnemonic_sentence: mnemonics,
                                                        new_passphrase: PASS })
        expect(upd).to be_correct_and_respond 204
        wallet = SHELLEY.wallets.get(@wid)
        expect(wallet['state']['status']).to eq 'ready'
      end
    end
  end

  describe "E2E Byron" do

    def test_byron_tx(source_wid, target_wid)
      amt = 1000000
      address = SHELLEY.addresses.list(target_wid)[0]['id']
      target_before = get_shelley_balances(target_wid)
      src_before = get_byron_balances(source_wid)

      tx_sent = BYRON.transactions.create(source_wid, PASS, [{ address => amt }])

      expect(tx_sent).to be_correct_and_respond 202
      expect(tx_sent.to_s).to include "pending"
      wait_for_tx_in_ledger(target_wid, tx_sent['id'])

      target_after = get_shelley_balances(target_wid)
      src_after = get_byron_balances(source_wid)
      fee = BYRON.transactions.get(source_wid, tx_sent['id'])['fee']['quantity']

      verify_ada_balance(src_after, src_before,
                         target_after, target_before,
                         amt, fee)
    end

    def test_byron_assets_tx(source_id, target_id)
      amt = 1
      address = SHELLEY.addresses.list(target_id)[1]['id']
      target_before = get_shelley_balances(target_id)
      src_before = get_byron_balances(source_id)
      payload = [{ "address" => address,
                  "amount" => { "quantity" => 0, "unit" => "lovelace" },
                  "assets" => [ { "policy_id" => ASSETS[0]["policy_id"],
                                  "asset_name" => ASSETS[0]["asset_name"],
                                  "quantity" => amt
                                },
                                { "policy_id" => ASSETS[1]["policy_id"],
                                  "asset_name" => ASSETS[1]["asset_name"],
                                  "quantity" => amt
                                }
                              ]
                  }
                 ]

      tx_sent = BYRON.transactions.create(source_id, PASS, payload)

      expect(tx_sent).to be_correct_and_respond 202
      expect(tx_sent.to_s).to include "pending"
      wait_for_tx_in_ledger(target_id, tx_sent['id'])

      target_after = get_shelley_balances(target_id)
      src_after = get_byron_balances(source_id)
      tx = BYRON.transactions.get(source_id, tx_sent['id'])
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
      expect(assets.to_s).to include ASSETS[0]["policy_id"]
      expect(assets.to_s).to include ASSETS[0]["asset_name"]
      expect(assets.to_s).to include ASSETS[0]["metadata"]["name"]
      expect(assets.to_s).to include ASSETS[1]["policy_id"]
      expect(assets.to_s).to include ASSETS[1]["asset_name"]
      expect(assets.to_s).to include ASSETS[1]["metadata"]["name"]
    end

    describe "Byron Transactions" do

      it "I can send transaction and funds are received, random -> shelley" do
        test_byron_tx(@wid_rnd, @target_id)
      end

      it "I can send transaction and funds are received, icarus -> shelley" do
        test_byron_tx(@wid_ic, @target_id)
      end
    end

    describe "Byron Migrations" do
      it "I can create migration plan byron -> shelley" do
        addrs = SHELLEY.addresses.list(@target_id).map { |a| a['id'] }

        plan = BYRON.migrations.plan(@wid_rnd, addrs)
        expect(plan).to be_correct_and_respond 202
        expect(plan['balance_selected']['assets']).not_to be []
        expect(plan['balance_leftover']).to eq ({ "ada" => { "quantity" => 0,
                                                             "unit" => "lovelace" },
                                                  "assets" => [] })
      end

      it "I can create migration plan icarus -> shelley" do
        addrs = SHELLEY.addresses.list(@target_id).map { |a| a['id'] }

        plan = BYRON.migrations.plan(@wid_ic, addrs)
        expect(plan).to be_correct_and_respond 202
        expect(plan['balance_selected']['assets']).not_to be []
        expect(plan['balance_leftover']).to eq ({ "ada" => { "quantity" => 0,
                                                             "unit" => "lovelace" },
                                                  "assets" => [] })
      end
    end

    describe "Native Assets" do

      it "I can list assets -> random" do
        assets = BYRON.assets.get @wid_rnd
        expect(assets).to be_correct_and_respond 200
        expect(assets.to_s).to include ASSETS[0]["policy_id"]
        expect(assets.to_s).to include ASSETS[0]["asset_name"]
        expect(assets.to_s).to include ASSETS[0]["metadata"]["name"]
        expect(assets.to_s).to include ASSETS[1]["policy_id"]
        expect(assets.to_s).to include ASSETS[1]["asset_name"]
        expect(assets.to_s).to include ASSETS[1]["metadata"]["name"]
      end

      it "I can list assets -> icarus" do
        assets = BYRON.assets.get @wid_ic
        expect(assets).to be_correct_and_respond 200
        expect(assets.to_s).to include ASSETS[0]["policy_id"]
        expect(assets.to_s).to include ASSETS[0]["asset_name"]
        expect(assets.to_s).to include ASSETS[0]["metadata"]["name"]
        expect(assets.to_s).to include ASSETS[1]["policy_id"]
        expect(assets.to_s).to include ASSETS[1]["asset_name"]
        expect(assets.to_s).to include ASSETS[1]["metadata"]["name"]
      end

      it "I can get native assets by policy_id -> random" do
        assets = BYRON.assets.get(@wid_rnd, policy_id = ASSETS[0]["policy_id"])
        expect(assets).to be_correct_and_respond 200
        expect(assets["policy_id"]).to eq ASSETS[0]["policy_id"]
        expect(assets["asset_name"]).to eq ASSETS[0]["asset_name"]
        expect(assets["metadata"]).to eq ASSETS[0]["metadata"]
        expect(assets["asset_name"]).not_to eq ASSETS[1]["asset_name"]
        expect(assets["metadata"]).not_to eq ASSETS[1]["metadata"]
      end

      it "I can get native assets by policy_id and asset_name -> random" do
        assets = BYRON.assets.get(@wid_rnd, policy_id = ASSETS[1]["policy_id"], asset_name = ASSETS[1]["asset_name"])
        expect(assets).to be_correct_and_respond 200
        expect(assets["policy_id"]).to eq ASSETS[1]["policy_id"]
        expect(assets["asset_name"]).to eq ASSETS[1]["asset_name"]
        expect(assets["metadata"]).to eq ASSETS[1]["metadata"]
        expect(assets["asset_name"]).not_to eq ASSETS[0]["asset_name"]
        expect(assets["metadata"]).not_to eq ASSETS[0]["metadata"]["name"]
      end

      it "I can send native assets tx and they are received (random -> shelley)" do
        test_byron_assets_tx(@wid_rnd, @target_id)
      end

      it "I can send native assets tx and they are received (icarus -> shelley)" do
        test_byron_assets_tx(@wid_ic, @target_id)
      end

    end
  end

  describe "E2E External transaction" do
    it "Single output transaction" do
      amt = 1000000
      address = SHELLEY.addresses.list(@target_id)[0]['id']
      target_before = get_shelley_balances(@target_id)
      src_before = get_shelley_balances(@wid)

      payment = [{ :address => address,
                 :amount => { :quantity => amt,
                           :unit => 'lovelace' }
               }]
      tx_constructed = SHELLEY.transactions.construct(@wid, payment)
      expect(tx_constructed).to be_correct_and_respond 202
      expected_fee = tx_constructed['fee']['quantity']
      tx_decoded = SHELLEY.transactions.decode(@wid, tx_constructed["transaction"])
      expect(tx_decoded).to be_correct_and_respond 202

      decoded_fee = tx_decoded['fee']['quantity']
      expect(expected_fee).to eq decoded_fee

      tx_signed = SHELLEY.transactions.sign(@wid, PASS, tx_constructed['transaction'])
      expect(tx_signed).to be_correct_and_respond 202

      tx_submitted = SHELLEY.transactions.submit(@wid, tx_signed['transaction'])
      expect(tx_submitted).to be_correct_and_respond 202
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
  end

  describe "E2E Migration" do
    it "I can migrate all funds back to fixture wallet" do
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

    end
  end

end
