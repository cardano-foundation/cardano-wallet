# frozen_string_literal: true

RSpec.describe 'Cardano Wallet E2E tests - Shared wallets', :all, :e2e, :shared do
  before(:all) do
    # shelley wallets
    @wid = create_fixture_wallet(:shelley)
    @target_id = create_target_wallet(:shelley)

    # shared wallets
    @wid_sha = create_fixture_wallet(:shared, :payment_cosigner0_all0, :delegation_cosigner0_all0)
    @wid_sha_cos0_all = create_fixture_wallet(:shared, :payment_cosigner0_all, :delegation_cosigner0_all)
    @wid_sha_cos1_all = create_fixture_wallet(:shared2, :payment_cosigner1_all, :delegation_cosigner1_all)
    @wid_sha_cos0_any = create_fixture_wallet(:shared, :payment_cosigner0_any, :delegation_cosigner0_any)
    cos0 = shared_acc_pubkey(@wid_sha_cos0_all)
    cos1 = shared_acc_pubkey(@wid_sha_cos1_all)
    patch_if_incomplete(@wid_sha_cos0_all, { 'cosigner#1' => cos1 }, { 'cosigner#1' => cos1 })
    patch_if_incomplete(@wid_sha_cos1_all, { 'cosigner#0' => cos0 }, { 'cosigner#0' => cos0 })
    patch_if_incomplete(@wid_sha_cos0_any, { 'cosigner#1' => cos1 }, { 'cosigner#1' => cos1 })

    @nightly_shared_wallets = [@wid_sha, @wid_sha_cos0_all, @wid_sha_cos1_all, @wid_sha_cos0_any]
    @nightly_shelley_wallets = [@wid, @target_id]
    wait_for_all_shelley_wallets(@nightly_shelley_wallets)
    wait_for_all_shared_wallets(@nightly_shared_wallets)
  end

  after(:each) do
    teardown
  end

  after(:all) do
    SHELLEY.stake_pools.quit(@target_id, PASS)
  end

  describe 'E2E Shared' do
    describe 'UTxOs' do
      it 'Fixture shared wallets have utxos' do
        @nightly_shared_wallets.each do |wid|
          utxo_stats = SHARED.wallets.utxo(wid)
          expect(utxo_stats).to be_correct_and_respond 200

          utxo_snap = SHARED.wallets.utxo_snapshot(wid)
          expect(utxo_snap).to be_correct_and_respond 200
          expect(utxo_snap['entries'].size).to be > 0
        end
      end
    end

    describe 'E2E Construct -> Sign -> Submit - multi signers' do
      it 'Cannot submit if partially signed - one cosigner, all' do
        amt = MIN_UTXO_VALUE_PURE_ADA * 2
        src_wid = @wid_sha_cos0_all
        target_wid = @target_id
        address = SHELLEY.addresses.list(target_wid)[1]['id']

        tx_constructed = SHARED.transactions.construct(src_wid, payment_payload(amt, address))
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(src_wid, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # cosigner0 signs
        tx_signed1 = SHARED.transactions.sign(src_wid, PASS, tx_constructed['transaction'])
        expect(tx_signed1).to be_correct_and_respond 202

        # cosigner0 submits
        tx_submitted = SHARED.transactions.submit(src_wid, tx_signed1['transaction'])
        expect(tx_submitted).to be_correct_and_respond 403
        expect(tx_submitted['code']).to eq 'missing_witnesses_in_transaction'
      end

      it 'Cannot submit if tx is foreign - two cosigners, all' do
        amt = MIN_UTXO_VALUE_PURE_ADA * 2
        src_wid = @wid_sha_cos0_all
        cosigner_wid = @wid_sha_cos1_all
        foreign_wid = @wid_sha
        target_wid = @target_id
        address = SHELLEY.addresses.list(target_wid)[1]['id']

        tx_constructed = SHARED.transactions.construct(src_wid, payment_payload(amt, address))
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(src_wid, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # cosigner0 signs
        tx_signed1 = SHARED.transactions.sign(src_wid, PASS, tx_constructed['transaction'])
        expect(tx_signed1).to be_correct_and_respond 202

        # cosigner1 signs
        tx_signed = SHARED.transactions.sign(cosigner_wid, PASS, tx_constructed['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        # foreign submits
        tx_submitted = SHARED.transactions.submit(foreign_wid, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 403
        expect(tx_submitted['code']).to eq 'foreign_transaction'
      end

      it 'Single output transaction - two cosigners, all' do
        amt = MIN_UTXO_VALUE_PURE_ADA * 2
        src_wid = @wid_sha_cos0_all
        cosigner_wid = @wid_sha_cos1_all
        target_wid = @target_id
        address = SHELLEY.addresses.list(target_wid)[1]['id']
        target_before = get_shelley_balances(target_wid)
        src_before = get_shared_balances(src_wid)

        tx_constructed = SHARED.transactions.construct(src_wid, payment_payload(amt, address))
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(src_wid, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # cosigner0 signs
        tx_signed1 = SHARED.transactions.sign(src_wid, PASS, tx_constructed['transaction'])
        expect(tx_signed1).to be_correct_and_respond 202

        # cosigner1 signs
        tx_signed = SHARED.transactions.sign(cosigner_wid, PASS, tx_signed1['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        # cosigner0 submits
        tx_submitted = SHARED.transactions.submit(src_wid, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(src_wid, tx_id, SHARED)
        verify_tx_status(src_wid, tx_id, 'in_ledger', SHARED)

        target_after = get_shelley_balances(target_wid)
        src_after = get_shared_balances(src_wid)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt, expected_fee)
        # tx history
        # on src wallet
        tx = SHARED.transactions.get(src_wid, tx_id)
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

        # on co-signer wid
        tx = SHARED.transactions.get(cosigner_wid, tx_id)
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
        txt = SHELLEY.transactions.get(target_wid, tx_id)
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
      end

      it 'Multi output transaction - two cosigners, all' do
        amt = MIN_UTXO_VALUE_PURE_ADA
        src_wid = @wid_sha_cos0_all
        cosigner_wid = @wid_sha_cos1_all
        target_wid = @target_id
        address = SHELLEY.addresses.list(target_wid)[1]['id']
        target_before = get_shelley_balances(target_wid)
        src_before = get_shared_balances(src_wid)
        payment = [{ address: address,
                     amount: { quantity: amt,
                               unit: 'lovelace' } },
                   { address: address,
                     amount: { quantity: amt,
                               unit: 'lovelace' } }]

        tx_constructed = SHARED.transactions.construct(src_wid, payment)
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(src_wid, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # cosigner0 signs
        tx_signed1 = SHARED.transactions.sign(src_wid, PASS, tx_constructed['transaction'])
        expect(tx_signed1).to be_correct_and_respond 202

        # cosigner1 signs
        tx_signed = SHARED.transactions.sign(cosigner_wid, PASS, tx_signed1['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        # cosigner1 submits
        tx_submitted = SHARED.transactions.submit(cosigner_wid, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(src_wid, tx_id, SHARED)
        verify_tx_status(cosigner_wid, tx_id, 'in_ledger', SHARED)

        target_after = get_shelley_balances(target_wid)
        src_after = get_shared_balances(src_wid)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt * 2, expected_fee)
        # tx history
        # on src wallet
        tx = SHARED.transactions.get(src_wid, tx_id)
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

        # on co-signer wid
        tx = SHARED.transactions.get(cosigner_wid, tx_id)
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
        txt = SHELLEY.transactions.get(target_wid, tx_id)
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
      end

      it 'Multi-assets transaction - two cosigners, all' do
        amt = 1
        amt_ada = 1_600_000
        src_wid = @wid_sha_cos0_all
        cosigner_wid = @wid_sha_cos1_all
        target_wid = @target_id
        address = SHELLEY.addresses.list(target_wid)[1]['id']
        target_before = get_shelley_balances(target_wid)
        src_before = get_shared_balances(src_wid)
        payment = [{ 'address' => address,
                     'amount' => { 'quantity' => amt_ada, 'unit' => 'lovelace' },
                     'assets' => [{ 'policy_id' => ASSETS[0]['policy_id'],
                                    'asset_name' => ASSETS[0]['asset_name'],
                                    'quantity' => amt },
                                  { 'policy_id' => ASSETS[1]['policy_id'],
                                    'asset_name' => ASSETS[1]['asset_name'],
                                    'quantity' => amt }] }]

        tx_constructed = SHARED.transactions.construct(src_wid, payment)
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(src_wid, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # cosigner0 signs
        tx_signed1 = SHARED.transactions.sign(src_wid, PASS, tx_constructed['transaction'])
        expect(tx_signed1).to be_correct_and_respond 202

        # cosigner1 signs
        tx_signed = SHARED.transactions.sign(cosigner_wid, PASS, tx_signed1['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        # cosigner0 submits
        tx_submitted = SHARED.transactions.submit(src_wid, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(src_wid, tx_id, SHARED)

        target_after = get_shelley_balances(target_wid)
        src_after = get_shared_balances(src_wid)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt_ada, expected_fee)
        # tx history
        # on src wallet
        tx = SHARED.transactions.get(src_wid, tx_id)
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

        # on co-signer wid
        tx = SHARED.transactions.get(cosigner_wid, tx_id)
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
        txt = SHELLEY.transactions.get(target_wid, tx_id)
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
      end

      it 'Validity intervals - two cosigners, all' do
        amt = MIN_UTXO_VALUE_PURE_ADA
        src_wid = @wid_sha_cos0_all
        cosigner_wid = @wid_sha_cos1_all
        target_wid = @target_id
        address = SHELLEY.addresses.list(target_wid)[1]['id']
        target_before = get_shelley_balances(target_wid)
        src_before = get_shared_balances(src_wid)
        inv_before = 500
        inv_hereafter = 5_000_000_000
        validity_interval = { 'invalid_before' => { 'quantity' => inv_before, 'unit' => 'slot' },
                              'invalid_hereafter' => { 'quantity' => inv_hereafter, 'unit' => 'slot' } }
        tx_constructed = SHARED.transactions.construct(cosigner_wid,
                                                       payment_payload(amt, address),
                                                       nil, # withdrawal
                                                       nil, # metadata
                                                       nil, # delegations
                                                       nil, # mint_burn
                                                       validity_interval)
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(src_wid, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # cosigner0 signs
        tx_signed1 = SHARED.transactions.sign(src_wid, PASS, tx_constructed['transaction'])
        expect(tx_signed1).to be_correct_and_respond 202

        # cosigner1 signs
        tx_signed = SHARED.transactions.sign(cosigner_wid, PASS, tx_signed1['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        # cosigner0 submits
        tx_submitted = SHARED.transactions.submit(src_wid, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(src_wid, tx_id, SHARED)

        target_after = get_shelley_balances(target_wid)
        src_after = get_shared_balances(src_wid)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt, expected_fee)
        # tx history
        # on src wallet
        tx = SHARED.transactions.get(src_wid, tx_id)
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

        # on co-signer wid
        tx = SHARED.transactions.get(cosigner_wid, tx_id)
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
        txt = SHELLEY.transactions.get(target_wid, tx_id)
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
      end

      it 'Only metadata - two cosigners, all' do
        src_wid = @wid_sha_cos0_all
        cosigner_wid = @wid_sha_cos1_all
        metadata = METADATA
        balance = get_shared_balances(src_wid)

        tx_constructed = SHARED.transactions.construct(src_wid,
                                                       nil, # payments
                                                       nil, # withdrawal
                                                       metadata)
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(src_wid, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # cosigner0 signs
        tx_signed1 = SHARED.transactions.sign(src_wid, PASS, tx_constructed['transaction'])
        expect(tx_signed1).to be_correct_and_respond 202

        # cosigner1 signs
        tx_signed = SHARED.transactions.sign(cosigner_wid, PASS, tx_signed1['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        # cosigner0 submits
        tx_submitted = SHARED.transactions.submit(src_wid, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(src_wid, tx_id, SHARED)

        # examine the tx in history
        # on src wallet
        tx = SHARED.transactions.get(src_wid, tx_id)
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
        new_balance = get_shared_balances(src_wid)
        expect(new_balance['available']).to eq(balance['available'] - expected_fee)
        expect(new_balance['total']).to eq(balance['total'] - expected_fee)
      end

      it 'Single output transaction - one cosigner, any' do
        amt = MIN_UTXO_VALUE_PURE_ADA * 2
        src_wid = @wid_sha_cos0_any
        target_wid = @target_id
        address = SHELLEY.addresses.list(target_wid)[1]['id']
        target_before = get_shelley_balances(target_wid)
        src_before = get_shared_balances(src_wid)

        tx_constructed = SHARED.transactions.construct(src_wid, payment_payload(amt, address))
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(src_wid, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee

        # cosigner0 signs
        tx_signed = SHARED.transactions.sign(src_wid, PASS, tx_constructed['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        # cosigner0 submits
        tx_submitted = SHARED.transactions.submit(src_wid, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(src_wid, tx_id, SHARED)

        target_after = get_shelley_balances(target_wid)
        src_after = get_shared_balances(src_wid)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt, expected_fee)
        # tx history
        # on src wallet
        tx = SHARED.transactions.get(src_wid, tx_id)
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
        txt = SHELLEY.transactions.get(target_wid, tx_id)
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
      end
    end
    describe 'E2E Construct -> Sign -> Submit - single signer' do
      it 'I can get min_utxo_value when contructing tx' do
        amt = 1
        tx_constructed = SHARED.transactions.construct(@wid_sha, payment_payload(amt))
        expect(tx_constructed.code).to eq 403
        expect(tx_constructed['code']).to eq 'utxo_too_small'
        required_minimum = tx_constructed['info']['tx_output_lovelace_required_minimum']['quantity']

        tx_constructed = SHARED.transactions.construct(@wid_sha, payment_payload(required_minimum))
        expect(tx_constructed).to be_correct_and_respond 202
      end

      it 'Single output transaction' do
        amt = MIN_UTXO_VALUE_PURE_ADA * 2
        address = SHELLEY.addresses.list(@target_id)[1]['id']
        target_before = get_shelley_balances(@target_id)
        src_before = get_shared_balances(@wid_sha)

        tx_constructed = SHARED.transactions.construct(@wid_sha, payment_payload(amt, address))
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(@wid_sha, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
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
        expect(tx_decoded['metadata']).to eq nil
        expect(tx_decoded['deposits_taken']).to eq []
        expect(tx_decoded['deposits_returned']).to eq []
        expect(tx_decoded['withdrawals']).to eq []
        expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
        expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
        expect(tx_decoded['certificates']).to eq []

        tx_signed = SHARED.transactions.sign(@wid_sha, PASS, tx_constructed['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        tx_submitted = SHARED.transactions.submit(@wid_sha, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid_sha, tx_id, SHARED)

        target_after = get_shelley_balances(@target_id)
        src_after = get_shared_balances(@wid_sha)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt, expected_fee)
        # tx history
        # on src wallet
        tx = SHARED.transactions.get(@wid_sha, tx_id)
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
      end

      it 'Multi output transaction' do
        amt = MIN_UTXO_VALUE_PURE_ADA
        address = SHELLEY.addresses.list(@target_id)[1]['id']
        target_before = get_shelley_balances(@target_id)
        src_before = get_shared_balances(@wid_sha)

        payment = [{ address: address,
                     amount: { quantity: amt,
                               unit: 'lovelace' } },
                   { address: address,
                     amount: { quantity: amt,
                               unit: 'lovelace' } }]
        tx_constructed = SHARED.transactions.construct(@wid_sha, payment)
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(@wid_sha, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
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
        expect(tx_decoded['metadata']).to eq nil
        expect(tx_decoded['deposits_taken']).to eq []
        expect(tx_decoded['deposits_returned']).to eq []
        expect(tx_decoded['withdrawals']).to eq []
        expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
        expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
        expect(tx_decoded['certificates']).to eq []

        tx_signed = SHARED.transactions.sign(@wid_sha, PASS, tx_constructed['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        tx_submitted = SHARED.transactions.submit(@wid_sha, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid_sha, tx_id, SHARED)

        target_after = get_shelley_balances(@target_id)
        src_after = get_shared_balances(@wid_sha)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt * 2, expected_fee)
        # tx history
        # on src wallet
        tx = SHARED.transactions.get(@wid_sha, tx_id)
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
      end

      it 'Multi-assets transaction' do
        amt = 1
        amt_ada = MIN_UTXO_VALUE_PURE_ADA * 2
        address = SHELLEY.addresses.list(@target_id)[1]['id']
        target_before = get_shelley_balances(@target_id)
        src_before = get_shared_balances(@wid_sha)

        payment = [{ 'address' => address,
                     'amount' => { 'quantity' => amt_ada, 'unit' => 'lovelace' },
                     'assets' => [{ 'policy_id' => ASSETS[0]['policy_id'],
                                    'asset_name' => ASSETS[0]['asset_name'],
                                    'quantity' => amt },
                                  { 'policy_id' => ASSETS[1]['policy_id'],
                                    'asset_name' => ASSETS[1]['asset_name'],
                                    'quantity' => amt }] }]
        tx_constructed = SHARED.transactions.construct(@wid_sha, payment)
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(@wid_sha, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
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
        expect(tx_decoded['metadata']).to eq nil
        expect(tx_decoded['deposits_taken']).to eq []
        expect(tx_decoded['deposits_returned']).to eq []
        expect(tx_decoded['withdrawals']).to eq []
        expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
        expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
        expect(tx_decoded['certificates']).to eq []

        tx_signed = SHARED.transactions.sign(@wid_sha, PASS, tx_constructed['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        tx_submitted = SHARED.transactions.submit(@wid_sha, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid_sha, tx_id, SHARED)

        target_after = get_shelley_balances(@target_id)
        src_after = get_shared_balances(@wid_sha)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt_ada, expected_fee)

        verify_asset_balance(src_after, src_before,
                             target_after, target_before,
                             amt)
        # tx history
        # on src wallet
        tx = SHARED.transactions.get(@wid_sha, tx_id)
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
      end

      it 'Validity intervals' do
        amt = MIN_UTXO_VALUE_PURE_ADA
        address = SHELLEY.addresses.list(@target_id)[1]['id']
        target_before = get_shelley_balances(@target_id)
        src_before = get_shared_balances(@wid_sha)
        inv_before = 500
        inv_hereafter = 5_000_000_000
        validity_interval = { 'invalid_before' => { 'quantity' => inv_before, 'unit' => 'slot' },
                              'invalid_hereafter' => { 'quantity' => inv_hereafter, 'unit' => 'slot' } }
        tx_constructed = SHARED.transactions.construct(@wid_sha,
                                                       payment_payload(amt, address),
                                                       nil, # withdrawal
                                                       nil, # metadata
                                                       nil, # delegations
                                                       nil, # mint_burn
                                                       validity_interval)
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(@wid_sha, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
        decoded_fee = tx_decoded['fee']['quantity']
        expect(expected_fee).to eq decoded_fee
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

        tx_signed = SHARED.transactions.sign(@wid_sha, PASS, tx_constructed['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        tx_submitted = SHARED.transactions.submit(@wid_sha, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid_sha, tx_id, SHARED)

        target_after = get_shelley_balances(@target_id)
        src_after = get_shared_balances(@wid_sha)

        verify_ada_balance(src_after, src_before,
                           target_after, target_before,
                           amt, expected_fee)
        # tx history
        # on src wallet
        tx = SHARED.transactions.get(@wid_sha, tx_id)
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
      end

      it 'Only metadata' do
        metadata = METADATA
        balance = get_shared_balances(@wid_sha)
        tx_constructed = SHARED.transactions.construct(@wid_sha,
                                                       nil, # payments
                                                       nil, # withdrawal
                                                       metadata)
        expect(tx_constructed).to be_correct_and_respond 202
        expected_fee = tx_constructed['fee']['quantity']

        # Can be decoded
        tx_decoded = SHARED.transactions.decode(@wid_sha, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        expect(tx_decoded['id'].size).to be 64
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

        tx_signed = SHARED.transactions.sign(@wid_sha, PASS, tx_constructed['transaction'])
        expect(tx_signed).to be_correct_and_respond 202

        tx_submitted = SHARED.transactions.submit(@wid_sha, tx_signed['transaction'])
        expect(tx_submitted).to be_correct_and_respond 202
        signed_decoded = SHARED.transactions.decode(@wid_sha, tx_signed['transaction'])
        expect(signed_decoded['witness_count']['verification_key']).to be >= 1
        expect(expected_fee).to eq signed_decoded['fee']['quantity']

        tx_id = tx_submitted['id']
        wait_for_tx_in_ledger(@wid_sha, tx_id, SHARED)

        # examine the tx in history
        # on src wallet
        tx = SHARED.transactions.get(@wid_sha, tx_id)
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
        new_balance = get_shared_balances(@wid_sha)
        expect(new_balance['available']).to eq(balance['available'] - expected_fee)
        expect(new_balance['total']).to eq(balance['total'] - expected_fee)
      end

      it 'Delegation (without submitting)' do
        # Delegation not yet implemented, only construct and sign in this tc
        # balance = get_shared_balances(@wid_sha)
        expected_deposit = CARDANO_CLI.protocol_params['stakeAddressDeposit']
        puts "Expected deposit #{expected_deposit}"

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

        tx_constructed = SHARED.transactions.construct(@wid_sha,
                                                       nil, # payment
                                                       nil, # withdrawal
                                                       nil, # metadata
                                                       delegation,
                                                       nil, # mint_burn
                                                       nil) # validity_interval
        # Check fee and deposit on joining
        tx_decoded = SHARED.transactions.decode(@wid_sha, tx_constructed['transaction'])
        expect(tx_decoded).to be_correct_and_respond 202

        deposit_taken = tx_constructed['coin_selection']['deposits_taken'].first['quantity']
        decoded_deposit_taken = tx_decoded['deposits_taken'].first['quantity']
        expect(deposit_taken).to eq decoded_deposit_taken
        expect(deposit_taken).to eq expected_deposit

        expected_fee = tx_constructed['fee']['quantity']
        decoded_fee = tx_decoded['fee']['quantity']
        expect(decoded_fee).to eq expected_fee
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
        expect(tx_decoded['deposits_returned']).to eq []
        expect(tx_decoded['withdrawals']).to eq []
        expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
        expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
        
        # Certificates
        expect(tx_decoded['certificates']).to include(have_key('certificate_type')).twice
        expect(tx_decoded['certificates']).to include(have_value('register_reward_account')).once
        expect(tx_decoded['certificates']).to include(have_value('join_pool')).once
        expect(tx_decoded['certificates']).to include(have_key('reward_account_path')).twice
        expect(tx_decoded['certificates']).to include(have_value(%w[1854H 1815H 0H 2 0])).twice
        expect(tx_decoded['certificates']).to include(have_key('pool')).once
        expect(tx_decoded['certificates']).to include(have_value(pool_id)).once

        tx_signed = SHARED.transactions.sign(@wid_sha, PASS, tx_constructed['transaction'])
        expect(tx_signed).to be_correct_and_respond 202
      end

      describe 'Minting and Burning' do
        it 'Can mint and then burn (without submitting)' do
          # Minting and Burning not yet implemented, only construct and sign in this tc
          # src_before = get_shared_balances(@wid_sha)
          policy_script1 = 'cosigner#0'
          policy_script2 = { 'all' => ['cosigner#0'] }
          policy_script3 = { 'any' => ['cosigner#0'] }

          # Minting:
          mint = [mint(asset_name('Token1'), 1000, policy_script1),
                  mint(asset_name('Token2'), 1000, policy_script2),
                  mint('', 1000, policy_script3)]

          tx_constructed = SHARED.transactions.construct(@wid_sha,
                                                         nil, # payment
                                                         nil, # withdrawal
                                                         nil, # metadata
                                                         nil, # delegation
                                                         mint)
          expect(tx_constructed).to be_correct_and_respond 202

          tx_decoded = SHARED.transactions.decode(@wid_sha, tx_constructed['transaction'])
          expect(tx_decoded).to be_correct_and_respond 202

          expected_fee = tx_constructed['fee']['quantity']
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
          expect(tx_decoded['metadata']).to eq nil
          expect(tx_decoded['deposits_taken']).to eq []
          expect(tx_decoded['deposits_returned']).to eq []
          expect(tx_decoded['withdrawals']).to eq []
          # TODO: mint / burn currently not decoded
          expect(tx_decoded['mint']).to eq({ 'tokens' => [] })
          expect(tx_decoded['burn']).to eq({ 'tokens' => [] })
          expect(tx_decoded['certificates']).to eq []

          tx_signed = SHARED.transactions.sign(@wid_sha, PASS, tx_constructed['transaction'])
          expect(tx_signed).to be_correct_and_respond 202
        end
      end
    end

    it 'I can receive transaction to shared wallet' do
      amt = 1
      amt_ada = 3_000_000
      address = SHARED.addresses.list(@wid_sha)[1]['id']
      target_before = get_shared_balances(@wid_sha)
      src_before = get_shelley_balances(@wid)

      payload = [{ 'address' => address,
                   'amount' => { 'quantity' => amt_ada, 'unit' => 'lovelace' },
                   'assets' => [{ 'policy_id' => ASSETS[0]['policy_id'],
                                  'asset_name' => ASSETS[0]['asset_name'],
                                  'quantity' => amt },
                                { 'policy_id' => ASSETS[1]['policy_id'],
                                  'asset_name' => ASSETS[1]['asset_name'],
                                  'quantity' => amt }] }]

      tx_sent = SHELLEY.transactions.create(@wid, PASS, payload)

      expect(tx_sent).to be_correct_and_respond 202
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

    it 'I can list transactions and limit response with query parameters' do
      wid = @wid_sha

      # get 3 txs
      txs = SHARED.transactions.list(wid, { max_count: 3 })
      expect(txs).to be_correct_and_respond 200
      expect(txs.size).to be 3

      last_tx_time =  txs.first['inserted_at']['time']
      first_tx_time = txs.last['inserted_at']['time']

      # get 2 txs
      txs = SHARED.transactions.list(wid, { max_count: 2 })
      expect(txs).to be_correct_and_respond 200
      expect(txs.size).to eq 2
      expect(txs.first['inserted_at']['time']).to eq last_tx_time

      # get 2 txs in ascending order
      txs = SHARED.transactions.list(wid, { max_count: 2, start: first_tx_time, order: 'ascending' })
      expect(txs).to be_correct_and_respond 200
      expect(txs.size).to eq 2
      expect(txs.first['inserted_at']['time']).to eq first_tx_time

      # get 2 txs in descending order with start and end time
      txs = SHARED.transactions.list(wid, { max_count: 2, start: first_tx_time, end: last_tx_time, order: 'descending' })
      expect(txs).to be_correct_and_respond 200
      expect(txs.size).to eq 2
      expect(txs.first['inserted_at']['time']).to eq last_tx_time
    end
  end

  describe 'E2E Migration' do
    it 'I can migrate all funds back to fixture shared wallet' do
      address = SHARED.addresses.list(@wid_sha)[0]['id']
      src_before = get_shelley_balances(@target_id)
      target_before = get_shared_balances(@wid_sha)

      migration = SHELLEY.migrations.migrate(@target_id, PASS, [address])
      tx_ids = migration.map { |m| m['id'] }
      fees = migration.map { |m| m['fee']['quantity'] }.sum
      amounts = migration.map { |m| m['amount']['quantity'] }.sum - fees
      tx_ids.each do |tx_id|
        wait_for_tx_in_ledger(@target_id, tx_id)
      end
      src_after = get_shelley_balances(@target_id)
      target_after = get_shared_balances(@wid_sha)
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
        txt = SHARED.transactions.get(@wid_sha, tx_id)
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
