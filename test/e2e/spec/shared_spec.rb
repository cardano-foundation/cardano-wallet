# frozen_string_literal: true

RSpec.describe CardanoWallet::Shared, :all, :shared do
  after(:each) do
    teardown
  end

  describe CardanoWallet::Shared::Wallets do
    describe 'Create wallets' do
      it 'I can create, get and delete wallet from mnemonics getting acc_xpub from cardano-address' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        script_template = { 'cosigners' =>
                              { 'cosigner#0' => acc_xpub },
                            'template' =>
                                { 'all' =>
                                   ['cosigner#0',
                                    { 'active_from' => 120 }] } }

        payload = { mnemonic_sentence: m24,
                    passphrase: PASS,
                    name: 'Shared wallet',
                    account_index: acc_ix,
                    payment_script_template: script_template,
                    delegation_script_template: script_template }

        wallet = WalletFactory.create(:shared, payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = SHARED.wallets.get(wid)
        expect(g).to be_correct_and_respond 200

        l = SHARED.wallets.list
        expect(l).to be_correct_and_respond 200

        expect(WalletFactory.delete(:shared, wid)).to be_correct_and_respond 204
      end

      it 'I can create, get and delete wallet from pub key getting acc_xpub from cardano-address' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")

        payment_script_template = { 'cosigners' =>
                                          { 'cosigner#0' => acc_xpub },
                                    'template' =>
                                            { 'all' =>
                                               ['cosigner#0',
                                                { 'active_from' => 120 }] } }

        delegation_script_template = { 'cosigners' =>
                                            { 'cosigner#0' => acc_xpub },
                                       'template' =>
                                            { 'all' =>
                                               ['cosigner#0',
                                                'cosigner#1'] } }
        payload = { account_public_key: acc_xpub,
                    passphrase: PASS,
                    name: 'Shared wallet',
                    account_index: acc_ix,
                    payment_script_template: payment_script_template,
                    delegation_script_template: delegation_script_template }

        wallet = WalletFactory.create(:shared, payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = SHARED.wallets.get(wid)
        expect(g).to be_correct_and_respond 200

        l = SHARED.wallets.list
        expect(l).to be_correct_and_respond 200

        expect(WalletFactory.delete(:shared, wid)).to be_correct_and_respond 204
      end

      it 'Cannot create wallet with different acc xpub - derived from different mnemonic sentence' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub_wrong = cardano_address_get_acc_xpub(CW.utils.mnemonic_sentence(24),
                                                      '1854H/1815H/0H')

        payment_script_template = { 'cosigners' =>
                                          { 'cosigner#0' => acc_xpub_wrong },
                                    'template' =>
                                            { 'all' =>
                                               ['cosigner#0'] } }

        delegation_script_template = { 'cosigners' =>
                                          { 'cosigner#0' => acc_xpub_wrong },
                                       'template' =>
                                            { 'all' =>
                                               ['cosigner#0',
                                                'cosigner#1'] } }
        payload = { mnemonic_sentence: m24,
                    passphrase: PASS,
                    name: 'Shared wallet',
                    account_index: acc_ix,
                    payment_script_template: payment_script_template,
                    payment_script_template: delegation_script_template }

        size = SHARED.wallets.list.size
        wallet = SHARED.wallets.create(payload)
        expect(wallet).to be_correct_and_respond 403

        l = SHARED.wallets.list
        expect(l).to be_correct_and_respond 200
        expect(l.size).to be size
      end

      it 'Cannot create wallet with different acc xpub - derived from different acc ix' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub_wrong = cardano_address_get_acc_xpub(m24, '1854H/1815H/255H')

        payment_script_template = { 'cosigners' =>
                                          { 'cosigner#0' => acc_xpub_wrong },
                                    'template' =>
                                            { 'all' =>
                                               ['cosigner#0'] } }

        delegation_script_template = { 'cosigners' =>
                                          { 'cosigner#0' => acc_xpub_wrong },
                                       'template' =>
                                            { 'all' =>
                                               ['cosigner#0',
                                                'cosigner#1'] } }
        payload = { mnemonic_sentence: m24,
                    passphrase: PASS,
                    name: 'Shared wallet',
                    account_index: acc_ix,
                    payment_script_template: payment_script_template,
                    payment_script_template: delegation_script_template }

        size = SHARED.wallets.list.size
        wallet = SHARED.wallets.create(payload)
        expect(wallet).to be_correct_and_respond 403

        l = SHARED.wallets.list
        expect(l).to be_correct_and_respond 200
        expect(l.size).to be size
      end

      it 'I can create incomplete wallet and update cosigners with acc_xpub from cardano-address' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, '1854H/1815H/0H')
        incomplete_wid = create_incomplete_shared_wallet(m24, '0H', acc_xpub)
        addr = SHARED.addresses.list(incomplete_wid)
        expect(addr).to be_correct_and_respond 200
        expect(addr.size).to eq 0

        acc_xpub_upd = cardano_address_get_acc_xpub(CW.utils.mnemonic_sentence(24),
                                                    '1854H/1815H/0H')

        update_payment = SHARED.wallets.update_payment_script(incomplete_wid,
                                                              'cosigner#1',
                                                              acc_xpub_upd)

        expect(update_payment).to be_correct_and_respond 200
        expect(SHARED.wallets.get(incomplete_wid)['state']['status']).to eq 'incomplete'
        expect(SHARED.wallets.get(incomplete_wid)).to be_correct_and_respond 200
        addr = SHARED.addresses.list(incomplete_wid)
        expect(addr).to be_correct_and_respond 200
        expect(addr.size).to eq 0

        update_delegation = SHARED.wallets.update_delegation_script(incomplete_wid,
                                                                    'cosigner#1',
                                                                    acc_xpub_upd)

        expect(update_delegation).to be_correct_and_respond 200
        eventually "The wallet is no longer 'incomplete'" do
          SHARED.wallets.get(incomplete_wid)['state']['status'] != 'incomplete'
        end
        expect(SHARED.wallets.list).to be_correct_and_respond 200
        addr = SHARED.addresses.list(incomplete_wid)
        expect(addr).to be_correct_and_respond 200
        expect(addr.size).to eq 20
      end

      it 'Create / update partially / get / list / delete' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, '1854H/1815H/0H')
        incomplete_wid = create_incomplete_shared_wallet(m24, '0H', acc_xpub)

        acc_xpub_upd = cardano_address_get_acc_xpub(CW.utils.mnemonic_sentence(24),
                                                    '1854H/1815H/0H')

        update_payment = SHARED.wallets.update_payment_script(incomplete_wid,
                                                              'cosigner#1',
                                                              acc_xpub_upd)

        expect(update_payment).to be_correct_and_respond 200

        expect(SHARED.wallets.get(incomplete_wid)).to be_correct_and_respond 200
        expect(SHARED.wallets.get(incomplete_wid)['state']['status']).to eq 'incomplete'

        expect(SHARED.wallets.list).to be_correct_and_respond 200

        expect(WalletFactory.delete(:shared, incomplete_wid)).to be_correct_and_respond 204
      end

      it 'Cannot update main cosigner' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, '1854H/1815H/0H')
        incomplete_wid = create_incomplete_shared_wallet(m24, '0H', acc_xpub)
        acc_xpub_upd = cardano_address_get_acc_xpub(CW.utils.mnemonic_sentence(24),
                                                    '1854H/1815H/0H')

        update_payment = SHARED.wallets.update_payment_script(incomplete_wid,
                                                              'cosigner#0',
                                                              acc_xpub_upd)

        expect(update_payment).to be_correct_and_respond 403

        update_delegation = SHARED.wallets.update_delegation_script(incomplete_wid,
                                                                    'cosigner#0',
                                                                    acc_xpub_upd)

        expect(update_delegation).to be_correct_and_respond 403
      end

      it "Cannot update cosigner with main cosigner's xpub" do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, '1854H/1815H/0H')
        incomplete_wid = create_incomplete_shared_wallet(m24, '0H', acc_xpub)

        update_payment = SHARED.wallets.update_payment_script(incomplete_wid,
                                                              'cosigner#1',
                                                              acc_xpub)

        expect(update_payment).to be_correct_and_respond 403

        update_delegation = SHARED.wallets.update_delegation_script(incomplete_wid,
                                                                    'cosigner#1',
                                                                    acc_xpub)

        expect(update_delegation).to be_correct_and_respond 403
      end

      it "I can create/get/list/delete wallet using cosigner: 'self' - from mnemonics" do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        script_template = { 'cosigners' =>
                              { 'cosigner#0' => 'self' },
                            'template' =>
                                { 'all' =>
                                   ['cosigner#0'] } }

        payload = { mnemonic_sentence: m24,
                    passphrase: PASS,
                    name: 'Shared wallet',
                    account_index: acc_ix,
                    payment_script_template: script_template,
                    delegation_script_template: script_template }

        wallet = WalletFactory.create(:shared, payload)
        expect(wallet).to be_correct_and_respond 201
        expect(wallet['state']['status']).to eq 'syncing'

        wid = wallet['id']
        g = SHARED.wallets.get(wid)
        expect(g).to be_correct_and_respond 200

        l = SHARED.wallets.list
        expect(l).to be_correct_and_respond 200
        expect(l.to_s).to include wid

        expect(WalletFactory.delete(:shared, wid)).to be_correct_and_respond 204
      end

      it "I can create/get/list/delete wallet using cosigner: 'self' - from pub key" do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        payment_script_template = { 'cosigners' =>
                                          { 'cosigner#0' => 'self' },
                                    'template' =>
                                            { 'all' =>
                                               ['cosigner#0',
                                                { 'active_from' => 120 }] } }

        delegation_script_template = { 'cosigners' =>
                                            { 'cosigner#0' => 'self' },
                                       'template' =>
                                            { 'all' =>
                                               ['cosigner#0',
                                                'cosigner#1'] } }
        payload = { account_public_key: acc_xpub,
                    passphrase: PASS,
                    name: 'Shared wallet',
                    account_index: acc_ix,
                    payment_script_template: payment_script_template,
                    delegation_script_template: delegation_script_template }

        wallet = WalletFactory.create(:shared, payload)
        expect(wallet).to be_correct_and_respond 201
        expect(wallet['state']['status']).to eq 'incomplete'

        wid = wallet['id']
        g = SHARED.wallets.get(wid)
        expect(g).to be_correct_and_respond 200

        l = SHARED.wallets.list
        expect(l).to be_correct_and_respond 200
        expect(l.to_s).to include wid

        expect(WalletFactory.delete(:shared, wid)).to be_correct_and_respond 204
      end

      describe 'Wallet id' do
        it 'Shared walletid with only spending template from cardano-addresses' do
          mnemonics = CW.utils.mnemonic_sentence(24)
          acc_ix = '0H'
          script_template = { 'cosigners' => { 'cosigner#0' => 'self' },
                              'template' =>
                                  { 'all' =>
                                     ['cosigner#0',
                                      { 'active_from' => 120 }] } }

          payload = { mnemonic_sentence: mnemonics,
                      passphrase: PASS,
                      name: 'Shared wallet',
                      account_index: acc_ix,
                      payment_script_template: script_template }

          wallet = WalletFactory.create(:shared, payload)
          expect(wallet).to be_correct_and_respond 201
          wid = wallet['id']

          # based on acct prv key
          template = '--spending "all [cosigner#0, active_from 120]"'
          root_xsk = CA.prv_key_from_recovery_phrase(mnemonics, 'Shared')
          acct_key = CA.key_child(root_xsk, "1854H/1815H/#{acc_ix}")
          ca_wid_acct_key = CA.key_walletid(acct_key, template)

          # based on pub key from acct prv key
          pub_key = CA.key_public(acct_key, with_chain_code = true)
          ca_wid_pub_key = CA.key_walletid(pub_key, template)

          # wallet id from cardano-wallet is the same
          expect(ca_wid_acct_key).to eq ca_wid_acct_key
          expect(wid).to eq ca_wid_acct_key
        end

        it 'Shared walletid with spending and delegation template from cardano-addresses' do
          mnemonics = CW.utils.mnemonic_sentence(24)
          acc_ix = '0H'
          script_template = { 'cosigners' => { 'cosigner#0' => 'self' },
                              'template' =>
                                  { 'all' =>
                                     ['cosigner#0',
                                      { 'active_from' => 120 }] } }

          delegation_template = { 'cosigners' => { 'cosigner#1' => 'self' },
                                  'template' =>
                                      { 'any' =>
                                         ['cosigner#0',
                                          'cosigner#1'] } }

          payload = { mnemonic_sentence: mnemonics,
                      passphrase: PASS,
                      name: 'Shared wallet',
                      account_index: acc_ix,
                      payment_script_template: script_template,
                      delegation_script_template: delegation_template }

          wallet = WalletFactory.create(:shared, payload)
          expect(wallet).to be_correct_and_respond 201
          wid = wallet['id']

          # based on acct prv key
          template = '--spending "all [cosigner#0, active_from 120]" --staking "any [cosigner#0, cosigner#1]"'
          root_xsk = CA.prv_key_from_recovery_phrase(mnemonics, 'Shared')
          acct_key = CA.key_child(root_xsk, "1854H/1815H/#{acc_ix}")
          ca_wid_acct_key = CA.key_walletid(acct_key, template)

          # based on pub key from acct prv key
          pub_key = CA.key_public(acct_key, with_chain_code = true)
          ca_wid_pub_key = CA.key_walletid(pub_key, template)

          # wallet id from cardano-wallet is the same
          expect(ca_wid_acct_key).to eq ca_wid_acct_key
          expect(wid).to eq ca_wid_acct_key
        end
      end
    end

    describe 'Addresses' do
      it 'Can list addresses on active shared wallet - from pub key' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        active_wid = create_active_shared_wallet(acc_xpub, acc_ix, 'self')

        a = SHARED.addresses.list(active_wid)
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 20

        a = SHARED.addresses.list(active_wid, { state: 'used' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        a = SHARED.addresses.list(active_wid, { state: 'unused' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 20
      end

      it 'Can list addresses on active shared wallet - from mnemonics' do
        m24 = CW.utils.mnemonic_sentence(24)
        active_wid = create_active_shared_wallet(m24, '0H', 'self')

        a = SHARED.addresses.list(active_wid)
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 20

        a = SHARED.addresses.list(active_wid, { state: 'used' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        a = SHARED.addresses.list(active_wid, { state: 'unused' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 20
      end

      it 'Lists empty addresses on incomplete shared wallet - from pub key' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        incomplete_wid = create_incomplete_shared_wallet(acc_xpub, acc_ix, 'self')

        a = SHARED.addresses.list(incomplete_wid)
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        a = SHARED.addresses.list(incomplete_wid, { state: 'used' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        a = SHARED.addresses.list(incomplete_wid, { state: 'unused' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        acc_xpub = cardano_address_get_acc_xpub(CW.utils.mnemonic_sentence(24), '1854H/1815H/0H')
        patch_incomplete_shared_wallet(incomplete_wid,
                                       { 'cosigner#1' => acc_xpub },
                                       { 'cosigner#1' => acc_xpub })

        a = SHARED.addresses.list(incomplete_wid)
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 20

        a = SHARED.addresses.list(incomplete_wid, { state: 'used' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        a = SHARED.addresses.list(incomplete_wid, { state: 'unused' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 20
      end

      it 'Lists empty addresses on incomplete shared wallet - from mnemonics' do
        m24 = CW.utils.mnemonic_sentence(24)
        incomplete_wid = create_incomplete_shared_wallet(m24, '0H', 'self')

        a = SHARED.addresses.list(incomplete_wid)
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        a = SHARED.addresses.list(incomplete_wid, { state: 'used' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        a = SHARED.addresses.list(incomplete_wid, { state: 'unused' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        acc_xpub = cardano_address_get_acc_xpub(CW.utils.mnemonic_sentence(24), '1854H/1815H/0H')
        patch_incomplete_shared_wallet(incomplete_wid,
                                       { 'cosigner#1' => acc_xpub },
                                       { 'cosigner#1' => acc_xpub })

        a = SHARED.addresses.list(incomplete_wid)
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 20

        a = SHARED.addresses.list(incomplete_wid, { state: 'used' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 0

        a = SHARED.addresses.list(incomplete_wid, { state: 'unused' })
        expect(a).to be_correct_and_respond 200
        expect(a.size).to be 20
      end
    end

    describe 'Public Keys' do
      matrix = {
        'utxo_internal' => 'addr_shared_vk',
        'utxo_external' => 'addr_shared_vk',
        'mutable_account' => 'stake_shared_vk'
      }
      matrix_h = {
        'utxo_internal' => 'addr_shared_vkh',
        'utxo_external' => 'addr_shared_vkh',
        'mutable_account' => 'stake_shared_vkh'
      }

      it 'Get public key - incomplete wallet from mnemonics' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, '1854H/1815H/0H')
        incomplete_wid = create_incomplete_shared_wallet(m24, '0H', acc_xpub)

        matrix.each do |role, addr_prefix|
          id = [*0..100_000].sample
          res = SHARED.keys.get_public_key(incomplete_wid, role, id)
          expect(res).to be_correct_and_respond 200
          expect(res.to_s).to include addr_prefix
        end

        matrix_h.each do |role, addr_prefix|
          id = [*0..100_000].sample
          res = SHARED.keys.get_public_key(incomplete_wid, role, id, { hash: true })
          expect(res).to be_correct_and_respond 200
          expect(res.to_s).to include addr_prefix
        end
      end

      it 'Get public key - incomplete wallet from acc pub key' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        incomplete_wid = create_incomplete_shared_wallet(acc_xpub, acc_ix, acc_xpub)

        matrix.each do |role, addr_prefix|
          id = [*0..100_000].sample
          res = SHARED.keys.get_public_key(incomplete_wid, role, id)
          expect(res).to be_correct_and_respond 200
          expect(res.to_s).to include addr_prefix
        end

        matrix_h.each do |role, addr_prefix|
          id = [*0..100_000].sample
          res = SHARED.keys.get_public_key(incomplete_wid, role, id, { hash: true })
          expect(res).to be_correct_and_respond 200
          expect(res.to_s).to include addr_prefix
        end
      end

      it 'Get public key - active wallet from mnemonics' do
        m24 = CW.utils.mnemonic_sentence(24)
        active_wid = create_incomplete_shared_wallet(m24, '11H', 'self')

        matrix.each do |role, addr_prefix|
          id = [*0..100_000].sample
          res = SHARED.keys.get_public_key(active_wid, role, id)
          expect(res).to be_correct_and_respond 200
          expect(res.to_s).to include addr_prefix
        end

        matrix_h.each do |role, addr_prefix|
          id = [*0..100_000].sample
          res = SHARED.keys.get_public_key(active_wid, role, id, { hash: true })
          expect(res).to be_correct_and_respond 200
          expect(res.to_s).to include addr_prefix
        end
      end

      it 'Get public key - active wallet from acc pub key' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        active_wid = create_active_shared_wallet(acc_xpub, acc_ix, 'self')

        matrix.each do |role, addr_prefix|
          id = [*0..100_000].sample
          res = SHARED.keys.get_public_key(active_wid, role, id)
          expect(res).to be_correct_and_respond 200
          expect(res.to_s).to include addr_prefix
        end

        matrix_h.each do |role, addr_prefix|
          id = [*0..100_000].sample
          res = SHARED.keys.get_public_key(active_wid, role, id, { hash: true })
          expect(res).to be_correct_and_respond 200
          expect(res.to_s).to include addr_prefix
        end
      end
    end

    describe 'Account Public Keys' do
      it 'Create account public key - incomplete wallet from mnemonics' do
        m24 = CW.utils.mnemonic_sentence(24)
        der_path = '1854H/1815H/0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, der_path)
        incomplete_wid = create_incomplete_shared_wallet(m24, '0H', acc_xpub)
        %w[0H 1H 2147483647H 44H].each do |index|
          payload = { passphrase: PASS, format: 'extended' }
          res = SHARED.keys.create_acc_public_key(incomplete_wid, index, payload)
          expect(res).to be_correct_and_respond 202
          expect(res.to_s).to include cardano_address_get_acc_xpub(m24,
                                                                   "1854H/1815H/#{index}",
                                                                   hex = false,
                                                                   'Shared')

          payload = { passphrase: PASS, format: 'non_extended' }
          res = SHARED.keys.create_acc_public_key(incomplete_wid, index, payload)
          expect(res).to be_correct_and_respond 202
          expect(res.to_s).to include cardano_address_get_acc_xpub(m24,
                                                                   "1854H/1815H/#{index}",
                                                                   hex = false,
                                                                   'Shared',
                                                                   '--without-chain-code')
        end
      end

      it 'Cannot create account public key - incomplete wallet from acc pub key' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        incomplete_wid = create_incomplete_shared_wallet(acc_xpub, acc_ix, 'self')
        %w[0H 1H 2147483647H 44H].each do |index|
          res = SHARED.keys.create_acc_public_key(incomplete_wid, index, { passphrase: PASS, format: 'extended' })
          expect(res).to be_correct_and_respond 403
          expect(res.to_s).to include 'no_root_key'

          res = SHARED.keys.create_acc_public_key(incomplete_wid, index, { passphrase: PASS, format: 'non_extended' })
          expect(res).to be_correct_and_respond 403
          expect(res.to_s).to include 'no_root_key'
        end
      end

      it 'Create account public key - active wallet from mnemonics' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, '1854H/1815H/0H')
        active_wid = create_active_shared_wallet(m24, '0H', acc_xpub)
        %w[0H 1H 2147483647H 44H].each do |index|
          res = SHARED.keys.create_acc_public_key(active_wid, index, { passphrase: PASS, format: 'extended' })
          expect(res).to be_correct_and_respond 202
          expect(res.to_s).to include cardano_address_get_acc_xpub(m24,
                                                                   "1854H/1815H/#{index}",
                                                                   hex = false,
                                                                   'Shared')

          res = SHARED.keys.create_acc_public_key(active_wid, index, { passphrase: PASS, format: 'non_extended' })
          expect(res).to be_correct_and_respond 202
          expect(res.to_s).to include cardano_address_get_acc_xpub(m24,
                                                                   "1854H/1815H/#{index}",
                                                                   hex = false,
                                                                   'Shared',
                                                                   '--without-chain-code')
        end
      end

      it 'Cannot create account public key - active wallet from acc pub key' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        active_wid = create_active_shared_wallet(acc_xpub, acc_ix, 'self')
        %w[0H 1H 2147483647H 44H].each do |index|
          res = SHARED.keys.create_acc_public_key(active_wid, index, { passphrase: PASS, format: 'extended' })
          expect(res).to be_correct_and_respond 403
          expect(res.to_s).to include 'no_root_key'

          res = SHARED.keys.create_acc_public_key(active_wid, index, { passphrase: PASS, format: 'non_extended' })
          expect(res).to be_correct_and_respond 403
          expect(res.to_s).to include 'no_root_key'
        end
      end

      it 'Get account public key - active wallet from mnemonics' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, '1854H/1815H/0H')
        active_wid = create_active_shared_wallet(m24, '0H', acc_xpub)

        res = SHARED.keys.get_acc_public_key(active_wid)
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_vk'

        res = SHARED.keys.get_acc_public_key(active_wid, { format: 'extended' })
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_xvk'

        res = SHARED.keys.get_acc_public_key(active_wid, { format: 'non_extended' })
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_vk'
      end

      it 'Get account public key - active wallet from acc pub key' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        active_wid = create_active_shared_wallet(acc_xpub, acc_ix, 'self')

        res = SHARED.keys.get_acc_public_key(active_wid)
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_vk'

        res = SHARED.keys.get_acc_public_key(active_wid, { format: 'extended' })
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_xvk'

        res = SHARED.keys.get_acc_public_key(active_wid, { format: 'non_extended' })
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_vk'
      end

      it 'Get account public key - incomplete wallet from mnemonics' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, '1854H/1815H/0H')
        incomplete_wid = create_incomplete_shared_wallet(m24, '0H', acc_xpub)

        res = SHARED.keys.get_acc_public_key(incomplete_wid)
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_vk'

        res = SHARED.keys.get_acc_public_key(incomplete_wid, { format: 'extended' })
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_xvk'

        res = SHARED.keys.get_acc_public_key(incomplete_wid, { format: 'non_extended' })
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_vk'
      end

      it 'Get account public key - incomplete wallet from acc pub key' do
        m24 = CW.utils.mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        incomplete_wid = create_incomplete_shared_wallet(acc_xpub, acc_ix, 'self')

        res = SHARED.keys.get_acc_public_key(incomplete_wid)
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_vk'

        res = SHARED.keys.get_acc_public_key(incomplete_wid, { format: 'extended' })
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_xvk'

        res = SHARED.keys.get_acc_public_key(incomplete_wid, { format: 'non_extended' })
        expect(res).to be_correct_and_respond 200
        expect(res.to_s).to include 'acct_shared_vk'
      end
    end
  end
end
