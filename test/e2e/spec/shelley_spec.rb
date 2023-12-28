# frozen_string_literal: true

RSpec.describe CardanoWallet::Shelley, :all, :shelley do
  after(:each) do
    teardown
  end

  describe CardanoWallet::Shelley::Wallets do
    it 'I can list wallets' do
      l = SHELLEY.wallets.list
      expect(l).to be_correct_and_respond 200
      size = l.size

      create_shelley_wallet
      l = SHELLEY.wallets.list
      expect(l).to be_correct_and_respond 200
      expect(l.size).to eq(size + 1)
    end

    it 'When wallet does not exist it gives 404' do
      wid = create_shelley_wallet
      WalletFactory.delete :shelley, wid
      g = SHELLEY.wallets.get wid
      expect(g).to be_correct_and_respond 404

      d = SHELLEY.wallets.delete wid
      expect(d).to be_correct_and_respond 404
    end

    describe 'Create wallets' do
      it 'I can create, get and delete wallet from mnemonics' do
        payload = { name: 'Wallet from mnemonic_sentence',
                    passphrase: 'Secure Passphrase',
                    mnemonic_sentence: CW.utils.mnemonic_sentence(15) }
        wallet = WalletFactory.create(:shelley, payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = SHELLEY.wallets.get(wid)
        expect(g).to be_correct_and_respond 200

        expect(WalletFactory.delete(:shelley, wid)).to be_correct_and_respond 204
      end

      it 'I can create, get and delete wallet from mnemonics / second factor' do
        payload = { name: 'Wallet from mnemonic_sentence',
                    passphrase: 'Secure Passphrase',
                    mnemonic_sentence: CW.utils.mnemonic_sentence(15),
                    mnemonic_second_factor: CW.utils.mnemonic_sentence(12) }
        wallet = WalletFactory.create(:shelley, payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = SHELLEY.wallets.get(wid)
        expect(g).to be_correct_and_respond 200
        expect(WalletFactory.delete(:shelley, wid)).to be_correct_and_respond 204
      end

      it 'I can set address pool gap' do
        pool_gap = 55
        payload = { name: 'Wallet from mnemonic_sentence',
                    passphrase: 'Secure Passphrase',
                    mnemonic_sentence: CW.utils.mnemonic_sentence(15),
                    address_pool_gap: pool_gap }
        wallet = WalletFactory.create(:shelley, payload)
        expect(wallet).to be_correct_and_respond 201
        addr = SHELLEY.addresses.list(wallet['id'])
        expect(addr).to be_correct_and_respond 200
        expect(addr.size).to eq pool_gap
      end

      it 'I can create, get and delete wallet from pub key' do
        payload = { name: 'Wallet from pub key',
                    account_public_key: 'b47546e661b6c1791452d003d375756dde6cac2250093ce4630f16b9b9c0ac87411337bda4d5bc0216462480b809824ffb48f17e08d95ab9f1b91d391e48e66b',
                    address_pool_gap: 20 }
        wallet = WalletFactory.create(:shelley, payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = SHELLEY.wallets.get(wid)
        expect(g).to be_correct_and_respond 200
        expect(WalletFactory.delete(:shelley, wid)).to be_correct_and_respond 204
      end

      describe 'Wallet id' do
        it 'I can get Shelley walletid using cardano-addresses' do
          mnemonics = CW.utils.mnemonic_sentence(24)
          wid = create_shelley_wallet('Shelley Wallet', mnemonics)

          # based on root prv key
          root_xsk = CA.prv_key_from_recovery_phrase(mnemonics, 'Shelley')
          ca_wid_root_xsk = CA.key_walletid(root_xsk)
          expect(wid).to eq ca_wid_root_xsk

          # based on pub key
          pub_key = CA.key_public(root_xsk)
          ca_wid_pub_key = CA.key_walletid(pub_key)
          expect(wid).to eq ca_wid_pub_key
        end

        it 'Shelley walletid is not based on acct key' do
          mnemonics = CW.utils.mnemonic_sentence(24)
          wid = create_shelley_wallet('Shelley Wallet', mnemonics)

          # based on acct prv key
          root_xsk = CA.prv_key_from_recovery_phrase(mnemonics, 'Shelley')
          acct_key = CA.key_child(root_xsk, '1852H/1815H/0H')
          ca_wid_acct_key = CA.key_walletid(acct_key)

          # based on pub key from acct prv key
          pub_key = CA.key_public(acct_key)
          ca_wid_pub_key = CA.key_walletid(pub_key)

          # wallet id from cardano-wallet is not the same
          expect(ca_wid_acct_key).to eq ca_wid_pub_key
          expect(wid).not_to eq ca_wid_acct_key
        end
      end
    end

    describe 'Update wallet' do
      it 'Can update_metadata' do
        new_name = 'New wallet name'
        w = SHELLEY.wallets
        id = create_shelley_wallet
        u = w.update_metadata(id, { name: new_name })
        expect(u).to be_correct_and_respond 200
        expect(w.get(id)['name']).to eq new_name
      end

      it 'Can update_passphrase' do
        w = SHELLEY.wallets
        id = create_shelley_wallet
        upd = w.update_passphrase(id, { old_passphrase: 'Secure Passphrase',
                                        new_passphrase: 'Securer Passphrase' })
        expect(upd).to be_correct_and_respond 204
      end

      it 'Cannot update_passphrase not knowing old pass' do
        w = SHELLEY.wallets
        id = create_shelley_wallet
        upd = w.update_passphrase(id, { old_passphrase: 'wrong-passphrase',
                                        new_passphrase: 'Securer Passphrase' })
        expect(upd).to be_correct_and_respond 403
        expect(upd.to_s).to include 'wrong_encryption_passphrase'
      end

      it 'Can update_passphrase, mnemonics' do
        w = SHELLEY.wallets
        mnemonics = CW.utils.mnemonic_sentence(24)
        id = create_shelley_wallet('Wallet', mnemonics)
        upd = w.update_passphrase(id, { mnemonic_sentence: mnemonics,
                                        new_passphrase: 'Securer Passphrase' })
        expect(upd).to be_correct_and_respond 204
      end

      it 'Can update_passphrase, mnemonics, mnemonic_second_factor' do
        w = SHELLEY.wallets
        mnemonics = CW.utils.mnemonic_sentence(24)
        mnemonic_second_factor = CW.utils.mnemonic_sentence(12)
        id = create_shelley_wallet('Wallet', mnemonics, mnemonic_second_factor)
        upd = w.update_passphrase(id, { mnemonic_sentence: mnemonics,
                                        mnemonic_second_factor: mnemonic_second_factor,
                                        new_passphrase: 'Securer Passphrase' })
        expect(upd).to be_correct_and_respond 204
      end

      it 'Cannot update_passphrase with wrong mnemonics' do
        w = SHELLEY.wallets
        mnemonics = CW.utils.mnemonic_sentence(24)
        wrong_mnemonics = CW.utils.mnemonic_sentence(24)
        id = create_shelley_wallet('Wallet', mnemonics)
        upd = w.update_passphrase(id, { mnemonic_sentence: wrong_mnemonics,
                                        new_passphrase: 'Securer Passphrase' })
        expect(upd).to be_correct_and_respond 403
        expect(upd.to_s).to include 'wrong_mnemonic'
      end

      it 'Cannot update_passphrase with wrong mnemonic_second_factor' do
        w = SHELLEY.wallets
        mnemonics = CW.utils.mnemonic_sentence(24)
        mnemonic_second_factor = CW.utils.mnemonic_sentence(12)
        wrong_mnemonic_second_factor = CW.utils.mnemonic_sentence(12)
        id = create_shelley_wallet('Wallet', mnemonics, mnemonic_second_factor)
        upd = w.update_passphrase(id, { mnemonic_sentence: mnemonics,
                                        mnemonic_second_factor: wrong_mnemonic_second_factor,
                                        new_passphrase: 'Securer Passphrase' })
        expect(upd).to be_correct_and_respond 403
        expect(upd.to_s).to include 'wrong_mnemonic'
      end

      it 'Cannot update_passphrase of wallet from pub key' do
        payload = { name: 'Wallet from pub key',
                    account_public_key: 'b47546e661b6c1791452d003d375756dde6cac2250093ce4630f16b9b9c0ac87411337bda4d5bc0216462480b809824ffb48f17e08d95ab9f1b91d391e48e66b',
                    address_pool_gap: 20 }
        wallet = WalletFactory.create(:shelley, payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        upd = SHELLEY.wallets.update_passphrase(wid, { old_passphrase: 'Secure Passphrase',
                                                       new_passphrase: 'Securer Passphrase' })
        expect(upd).to be_correct_and_respond 403
        expect(upd.to_s).to include 'no_root_key'
      end

      it 'Can update_passphrase of wallet from pub key using mnemonics from which pub key is derived' do
        mnemonics = CW.utils.mnemonic_sentence(24)
        root_xsk = CA.prv_key_from_recovery_phrase(mnemonics, 'Shelley')
        acct_key = CA.key_child(root_xsk, '1852H/1815H/0H')
        pub_key = CA.key_public(acct_key)
        acc_pub_key_base16 = bech32_to_base16(pub_key)

        payload = { name: 'Wallet from pub key',
                    account_public_key: acc_pub_key_base16,
                    address_pool_gap: 20 }
        wallet = WalletFactory.create(:shelley, payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']

        # I can update passphrase using mnemonics
        upd = SHELLEY.wallets.update_passphrase(wid, { mnemonic_sentence: mnemonics,
                                                       new_passphrase: 'Secure Passphrase' })
        expect(upd).to be_correct_and_respond 204

        # Once password is set I can perform passphrase-protected operations,
        # like update passphrase using old passprase
        upd2 = SHELLEY.wallets.update_passphrase(wid, { old_passphrase: 'Secure Passphrase',
                                                        new_passphrase: 'Securer Passphrase' })
        expect(upd2).to be_correct_and_respond 204
      end

      it 'Cannot update_passphrase of wallet from pub key using wrong mnemonics' do
        mnemonics = CW.utils.mnemonic_sentence(24)
        root_xsk = CA.prv_key_from_recovery_phrase(mnemonics, 'Shelley')
        acct_key = CA.key_child(root_xsk, '1852H/1815H/0H')
        pub_key = CA.key_public(acct_key)
        acc_pub_key_base16 = bech32_to_base16(pub_key)

        payload = { name: 'Wallet from pub key',
                    account_public_key: acc_pub_key_base16,
                    address_pool_gap: 20 }
        wallet = WalletFactory.create(:shelley, payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        wrong_mnemonics = CW.utils.mnemonic_sentence(24)
        upd = SHELLEY.wallets.update_passphrase(wid, { mnemonic_sentence: wrong_mnemonics,
                                                       new_passphrase: 'Securer Passphrase' })
        expect(upd).to be_correct_and_respond 403
        expect(upd.to_s).to include 'wrong_mnemonic'
      end
    end

    it 'Can see utxo' do
      id = create_shelley_wallet
      utxo = SHELLEY.wallets.utxo(id)
      expect(utxo).to be_correct_and_respond 200
    end

    it 'Can see utxo snapshot' do
      id = create_shelley_wallet
      utxo = SHELLEY.wallets.utxo_snapshot(id)
      expect(utxo).to be_correct_and_respond 200
    end
  end

  describe CardanoWallet::Shelley::Addresses do
    it 'Can list addresses' do
      id = create_shelley_wallet
      shelley_addr = SHELLEY.addresses
      addresses = shelley_addr.list id
      expect(addresses).to be_correct_and_respond 200
      expect(addresses.size).to eq 20
      addresses.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['1852H', '1815H', '0H', '0', i.to_s]
      end

      addresses_unused = shelley_addr.list id, { state: 'used' }
      expect(addresses_unused).to be_correct_and_respond 200
      expect(addresses_unused.size).to eq 0

      addresses_unused = shelley_addr.list id, { state: 'unused' }
      expect(addresses_unused).to be_correct_and_respond 200
      expect(addresses_unused.size).to eq 20
      addresses_unused.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['1852H', '1815H', '0H', '0', i.to_s]
      end
    end
  end

  describe CardanoWallet::Shelley::CoinSelections do
    it 'I could trigger random coin selection - if had money' do
      wid = create_shelley_wallet
      addresses = SHELLEY.addresses.list(wid)
      addr_amount = [
        { addresses[0]['id'] => MIN_UTXO_VALUE_PURE_ADA },
        { addresses[1]['id'] => MIN_UTXO_VALUE_PURE_ADA }
      ]

      rnd = SHELLEY.coin_selections.random wid, addr_amount
      expect(rnd).to be_correct_and_respond 403
      expect(rnd.to_s).to include 'no_utxos_available'
    end
  end

  describe CardanoWallet::Shelley::Transactions do
    it 'I could get a tx if I had proper id' do
      wid = create_shelley_wallet
      txs = SHELLEY.transactions
      g = txs.get(wid, TXID)
      expect(g).to be_correct_and_respond 404
      expect(g.to_s).to include 'no_such_transaction'
    end

    it 'Can list transactions' do
      id = create_shelley_wallet
      txs = SHELLEY.transactions
      expect(txs.list(id)).to be_correct_and_respond 200
      expect(txs.list(id, { max_count: 1 })).to be_correct_and_respond 200
      expect(txs.list(id, { start: '2012-09-25T10:15:00Z',
                            end: '2016-11-21T10:15:00Z',
                            order: 'ascending',
                            max_count: 10 })).to be_correct_and_respond 200
      expect(txs.list(id, { order: 'bad_order' })).to be_correct_and_respond 400
      expect(txs.list(id, { max_count: 'bad_count' })).to be_correct_and_respond 400
    end

    it 'I could create transaction - if I had money' do
      id = create_shelley_wallet
      target_id = create_shelley_wallet
      address = SHELLEY.addresses.list(target_id)[0]['id']
      txs = SHELLEY.transactions
      amt = [{ address => 1_000_000 }]

      tx_sent = txs.create(id, PASS, amt)
      expect(tx_sent).to be_correct_and_respond 403
      expect(tx_sent.to_s).to include 'no_utxos_available'
    end

    it 'I could create transaction using rewards - if I had money' do
      id = create_shelley_wallet
      target_id = create_shelley_wallet
      address = SHELLEY.addresses.list(target_id)[0]['id']
      txs = SHELLEY.transactions
      amt = [{ address => 1_000_000 }]

      tx_sent = txs.create(id, PASS, amt, 'self')
      expect(tx_sent).to be_correct_and_respond 403
      expect(tx_sent.to_s).to include 'no_utxos_available'
    end

    it 'I could estimate transaction fee - if I had money' do
      id = create_shelley_wallet
      target_id = create_shelley_wallet
      address = SHELLEY.addresses.list(target_id)[0]['id']
      amt = [{ address => 1_000_000 }]

      txs = SHELLEY.transactions

      fees = txs.payment_fees(id, amt)
      expect(fees).to be_correct_and_respond 403
      expect(fees.to_s).to include 'no_utxos_available'

      fees = txs.payment_fees(id, amt, 'self')
      expect(fees).to be_correct_and_respond 403
      expect(fees.to_s).to include 'no_utxos_available'

      metadata = { '0' => { 'string' => 'cardano' },
                   '1' => { 'int' => 14 },
                   '2' => { 'bytes' => '2512a00e9653fe49a44a5886202e24d77eeb998f' },
                   '3' => { 'list' => [{ 'int' => 14 }, { 'int' => 42 }, { 'string' => '1337' }] },
                   '4' => { 'map' => [{ 'k' => { 'string' => 'key' }, 'v' => { 'string' => 'value' } },
                                      { 'k' => { 'int' => 14 }, 'v' => { 'int' => 42 } }] } }

      fees = txs.payment_fees(id, amt, 'self', metadata)
      expect(fees).to be_correct_and_respond 403
      expect(fees.to_s).to include 'no_utxos_available'
    end

    it 'I could forget transaction' do
      id = create_shelley_wallet
      txs = SHELLEY.transactions
      res = txs.forget(id, TXID)
      expect(res).to be_correct_and_respond 404
    end
  end

  describe CardanoWallet::Shelley::StakePools do
    after(:each) do
      settings = CW.misc.settings
      settings.update({ pool_metadata_source: 'none' })
    end

    it 'I can list stake keys' do
      id = create_shelley_wallet
      stake_keys = SHELLEY.stake_pools.list_stake_keys(id)
      expect(stake_keys).to be_correct_and_respond 200
      expect(stake_keys['foreign'].size).to eq 0
      expect(stake_keys['ours'].size).to eq 1
      expect(stake_keys['ours'].first['stake']).to eq({ 'quantity' => 0, 'unit' => 'lovelace' })
      expect(stake_keys['none']['stake']).to eq({ 'quantity' => 0, 'unit' => 'lovelace' })
      expect(stake_keys['ours'].first['delegation']).to eq({ 'next' => [],
                                                             'active' =>
                                                             { 'status' => 'not_delegating' } })
    end

    describe 'ADP-634 - Pool metadata is updated when settings are updated', :offchain, :smash do
      it 'pool_metadata_source = direct <> none' do
        settings = CW.misc.settings
        pools = SHELLEY.stake_pools

        s = settings.update({ pool_metadata_source: 'direct' })
        expect(s).to be_correct_and_respond 204

        eventually "Pools have metadata when 'pool_metadata_source' => 'direct'" do
          pools.list({ stake: 1000 }).any? { |p| p['metadata'] }
        end

        s = settings.update({ pool_metadata_source: 'none' })
        expect(s).to be_correct_and_respond 204

        eventually "Pools have no metadata when 'pool_metadata_source' => 'none'" do
          pools.list({ stake: 1000 }).none? { |p| p['metadata'] }
        end
      end

      it "pool_metadata_source = #{ENV.fetch('TESTS_E2E_SMASH', nil)} <> none" do
        skip 'Smash is not set up yet on preview / preprod'
        settings = CW.misc.settings
        pools = SHELLEY.stake_pools
        s = settings.update({ pool_metadata_source: ENV.fetch('TESTS_E2E_SMASH', nil) })
        expect(s).to be_correct_and_respond 204

        eventually "Pools have metadata when 'pool_metadata_source' => '#{ENV.fetch('TESTS_E2E_SMASH', nil)}'" do
          pools.list({ stake: 1000 }).any? { |p| p['metadata'] }
        end

        s = settings.update({ pool_metadata_source: 'none' })
        expect(s).to be_correct_and_respond 204

        eventually "Pools have no metadata when 'pool_metadata_source' => 'none'" do
          pools.list({ stake: 1000 }).none? { |p| p['metadata'] }
        end
      end
    end

    describe 'Stake Pools GC Maintenance' do
      matrix = [{ 'direct' => 'not_applicable' },
                { 'none' => 'not_applicable' }]
      matrix.each do |tc|
        it "GC metadata maintenance action on metadata source #{tc}" do
          settings = CW.misc.settings
          pools = SHELLEY.stake_pools

          s = settings.update({ pool_metadata_source: tc.keys.first })
          expect(s).to be_correct_and_respond 204

          t = pools.trigger_maintenance_actions({ maintenance_action: 'gc_stake_pools' })
          expect(t).to be_correct_and_respond 204

          eventually "Maintenance action has status = #{tc.values.first}" do
            r = pools.view_maintenance_actions
            (r.code == 200) && (r.to_s.include? tc.values.first)
          end
        end
      end
    end
    describe 'Stake Pools GC Maintenance' do
      matrix = [{ ENV.fetch('TESTS_E2E_SMASH', nil) => 'has_run' }]
      matrix.each do |tc|
        it "GC metadata maintenance action on metadata source #{tc}" do
          pending "GC metadata maintenance action on metadata source #{ENV.fetch('TESTS_E2E_SMASH', nil)}"
          settings = CW.misc.settings
          pools = SHELLEY.stake_pools

          s = settings.update({ pool_metadata_source: tc.keys.first })
          expect(s).to be_correct_and_respond 204

          t = pools.trigger_maintenance_actions({ maintenance_action: 'gc_stake_pools' })
          expect(t).to be_correct_and_respond 204

          eventually "Maintenance action has status = #{tc.values.first}" do
            r = pools.view_maintenance_actions
            (r.code == 200) && (r.to_s.include? tc.values.first)
          end
        end
      end
    end
    it 'I could quit stake pool - if I was delegating' do
      id = create_shelley_wallet

      pools = SHELLEY.stake_pools
      quit = pools.quit(id, PASS)
      expect(quit).to be_correct_and_respond 403
      expect(quit.to_s).to include 'not_delegating_to'
    end
  end

  describe CardanoWallet::Shelley::Migrations do
    it 'I could create migration plan' do
      id = create_shelley_wallet
      target_id = create_shelley_wallet
      addrs = SHELLEY.addresses.list(target_id).map { |a| a['id'] }

      plan = SHELLEY.migrations.plan(id, addrs)
      expect(plan).to be_correct_and_respond 403
      expect(plan.to_s).to include 'nothing_to_migrate'
    end

    it 'I could migrate all my funds' do
      id = create_shelley_wallet
      target_id = create_shelley_wallet
      addrs = SHELLEY.addresses.list(target_id).map { |a| a['id'] }
      migr = SHELLEY.migrations.migrate(id, PASS, addrs)
      expect(migr).to be_correct_and_respond 403
      expect(migr.to_s).to include 'nothing_to_migrate'
    end
  end

  describe CardanoWallet::Shelley::Keys do
    it 'Get signed metadata' do
      wid = create_shelley_wallet
      %w[utxo_internal utxo_external mutable_account].each do |role|
        id = [*0..100_000].sample
        res = SHELLEY.keys.sign_metadata(wid,
                                         role,
                                         id,
                                         'Secure Passphrase',
                                         { '0' => { 'string' => 'cardano' } })
        puts "#{wid}/#{role}/#{id}"
        expect(res).to respond_with 200
      end
    end

    it 'Get public key' do
      wid = create_shelley_wallet
      %w[utxo_internal utxo_external mutable_account].each do |role|
        id = [*0..100_000].sample
        res = SHELLEY.keys.get_public_key(wid, role, id)
        puts "#{wid}/#{role}/#{id}"
        expect(res).to be_correct_and_respond 200
      end
    end

    it 'Create account public key - extended' do
      m24 = CW.utils.mnemonic_sentence(24)
      wid = create_shelley_wallet('Wallet', m24)
      %w[0H 1H 2147483647H 44H].each do |index|
        payload = { passphrase: PASS, format: 'extended' }
        res = SHELLEY.keys.create_acc_public_key(wid, index, payload)
        expect(res).to be_correct_and_respond 202
        expect(res.parsed_response).to eq cardano_address_get_acc_xpub(m24,
                                                                       "1852H/1815H/#{index}",
                                                                       'Shelley',
                                                                       '--with-chain-code',
                                                                       hex: false)
      end
    end

    it 'Create account public key - non_extended' do
      m24 = CW.utils.mnemonic_sentence(24)
      wid = create_shelley_wallet('Wallet', m24)
      %w[0H 1H 2147483647H 44H].each do |index|
        payload = { passphrase: PASS, format: 'non_extended' }
        res = SHELLEY.keys.create_acc_public_key(wid, index, payload)
        expect(res.parsed_response).to eq cardano_address_get_acc_xpub(m24,
                                                                       "1852H/1815H/#{index}",
                                                                       'Shelley',
                                                                       '--without-chain-code',
                                                                       hex: false)
      end
    end

    it 'Create account public key - extended with purpose' do
      m24 = CW.utils.mnemonic_sentence(24)
      wid = create_shelley_wallet('Wallet', m24)
      %w[0H 1H 2147483647H 1854H].each do |index_purpose|
        payload = { passphrase: PASS, format: 'extended', purpose: index_purpose }
        res = SHELLEY.keys.create_acc_public_key(wid, index_purpose, payload)
        expect(res).to be_correct_and_respond 202
        type_for_cardano_address = index_purpose == '1854H' ? 'Shared' : 'Shelley'
        expect(res.parsed_response).to eq cardano_address_get_acc_xpub(m24,
                                                                       "#{index_purpose}/1815H/#{index_purpose}",
                                                                       type_for_cardano_address,
                                                                       '--with-chain-code',
                                                                       hex: false)
      end
    end

    it 'Create account public key - non_extended with purpose' do
      m24 = CW.utils.mnemonic_sentence(24)
      wid = create_shelley_wallet('Wallet', m24)
      %w[0H 1H 2147483647H 1854H].each do |index_purpose|
        payload = { passphrase: PASS, format: 'non_extended', purpose: index_purpose }
        res = SHELLEY.keys.create_acc_public_key(wid, index_purpose, payload)
        expect(res).to be_correct_and_respond 202
        type_for_cardano_address = index_purpose == '1854H' ? 'Shared' : 'Shelley'
        expect(res.parsed_response).to eq cardano_address_get_acc_xpub(m24,
                                                                       "#{index_purpose}/1815H/#{index_purpose}",
                                                                       type_for_cardano_address,
                                                                       '--without-chain-code',
                                                                       hex: false)
      end
    end

    it 'Get account public key - wallet from acc pub key' do
      payload = { name: 'Wallet from pub key 2',
                  account_public_key: 'b47546e661b6c1791452d003d375756dde6cac2250093ce4630f16b9b9c0ac87411337bda4d5bc0216462480b809824ffb48f17e08d95ab9f1b91d391e48e66b',
                  address_pool_gap: 20 }
      wallet = WalletFactory.create(:shelley, payload)
      expect(wallet).to be_correct_and_respond 201

      res = SHELLEY.keys.get_acc_public_key(wallet['id'], { format: 'non_extended' })
      expect(res).to be_correct_and_respond 200
      expect(res.parsed_response).to include 'acct_vk'
    end

    it 'Get account public key - wallet from mnemonics' do
      m24 = CW.utils.mnemonic_sentence(24)
      wid = create_shelley_wallet('Wallet', m24)

      # Get account pub key from the wallet
      w_acct_key = SHELLEY.keys.get_acc_public_key(wid, { format: 'extended' })
      expect(w_acct_key).to be_correct_and_respond 200

      # Get equivalent account pub key using cardano-addresses
      root_xsk = CA.prv_key_from_recovery_phrase(m24, 'Shelley')
      acct_key = CA.key_child(root_xsk, '1852H/1815H/0H')
      pub_key = CA.key_public(acct_key)

      expect(pub_key).to eq w_acct_key.parsed_response
    end

    it 'Get account public key (mnemonic_snd_factor)' do
      m24 = CW.utils.mnemonic_sentence(24)
      m12 = CW.utils.mnemonic_sentence(12)
      wid = create_shelley_wallet('Wallet', m24, m12)

      # Get account pub key from the wallet
      w_acct_key = SHELLEY.keys.get_acc_public_key(wid, { format: 'extended' })
      expect(w_acct_key).to be_correct_and_respond 200

      # Get equivalent account pub key using cardano-addresses
      pub_key = Dir.mktmpdir do |dir|
        sndfactor_file = File.join(dir, 'sndfactor.prv')
        File.write(sndfactor_file, m12.join(' '))
        root_xsk = CA.prv_key_from_recovery_phrase(m24, "Shelley --passphrase from-mnemonic --from-file #{sndfactor_file}")
        acct_key = CA.key_child(root_xsk, '1852H/1815H/0H')
        CA.key_public(acct_key)
      end

      expect(pub_key).to eq w_acct_key.parsed_response
    end

    it "I can create and get policy key and it's hash" do
      wid = create_shelley_wallet
      created = SHELLEY.keys.create_policy_key(wid, PASS, { hash: true })
      expect(created).to be_correct_and_respond 202
      expect(created.to_s).to include 'policy_vkh'

      get = SHELLEY.keys.get_policy_key(wid, { hash: true })
      expect(get).to be_correct_and_respond 200

      expect(get.to_s).to eq created.to_s

      created = SHELLEY.keys.create_policy_key(wid, PASS)
      expect(created).to be_correct_and_respond 202
      expect(created.to_s).to include 'policy_vk'
      expect(created.to_s).not_to include 'policy_vkh'

      get = SHELLEY.keys.get_policy_key(wid)
      expect(get).to be_correct_and_respond 200

      expect(get.to_s).to eq created.to_s
    end

    describe 'Policy Id' do
      matrix = [
        ['cosigner#0', 202],
        [{ all: ['cosigner#0'] }, 202],
        [{ any: ['cosigner#0'] }, 202],
        [{ some: { at_least: 1, from: ['cosigner#0'] } }, 202],
        [{ all: ['cosigner#0', { active_from: 120 }] }, 202],
        ['cosigner#1', 403],
        [{ all: ['cosigner#0', 'cosigner#1'] }, 403]
      ]
      matrix.each do |m|
        template = m[0]
        code = m[1]
        it "Script template = #{template} gives #{code}" do
          wid = create_shelley_wallet
          created = SHELLEY.keys.create_policy_id(wid, template)
          expect(created).to be_correct_and_respond code
        end
      end
    end
  end
end
