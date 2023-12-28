# frozen_string_literal: true

RSpec.describe CardanoWallet::Byron, :all, :byron do
  after(:each) do
    teardown
  end

  describe CardanoWallet::Byron::Wallets do
    it 'I can list byron wallets' do
      l = BYRON.wallets.list
      expect(l).to be_correct_and_respond 200
    end

    it 'I could get a wallet' do
      g = BYRON.wallets.get 'db66f3d0d796c6aa0ad456a36d5a3ee88d62bd5d'
      expect(g).to be_correct_and_respond 404
    end

    it 'I could delete a wallet' do
      g = BYRON.wallets.delete 'db66f3d0d796c6aa0ad456a36d5a3ee88d62bd5d'
      expect(g).to be_correct_and_respond 404
    end

    it 'I can create, get and delete byron icarus wallet from mnemonics' do
      payload = { style: 'icarus',
                  name: 'Wallet from mnemonic_sentence',
                  passphrase: 'Secure Passphrase',
                  mnemonic_sentence: CW.utils.mnemonic_sentence(15) }
      wallet = WalletFactory.create(:byron, payload)
      expect(wallet).to be_correct_and_respond 201

      wid = wallet['id']
      expect(BYRON.wallets.get(wid)).to be_correct_and_respond 200
      expect(WalletFactory.delete(:byron, wid)).to be_correct_and_respond 204
    end

    it 'I can create, get and delete byron random wallet from mnemonics' do
      payload = { style: 'random',
                  name: 'Wallet from mnemonic_sentence',
                  passphrase: 'Secure Passphrase',
                  mnemonic_sentence: CW.utils.mnemonic_sentence(12) }
      wallet = WalletFactory.create(:byron, payload)
      expect(wallet).to be_correct_and_respond 201

      wid = wallet['id']
      expect(BYRON.wallets.get(wid)).to be_correct_and_respond 200
      expect(WalletFactory.delete(:byron, wid)).to be_correct_and_respond 204
    end

    describe 'Update wallet' do
      matrix = %w[random icarus]
      matrix.each do |wallet_style|
        it "Can update_metadata of #{wallet_style} wallet" do
          w = BYRON.wallets
          id = create_byron_wallet(wallet_style)
          u = w.update_metadata(id, { name: 'New wallet name' })
          expect(u).to be_correct_and_respond 200
        end

        it "Can update_passphrase of #{wallet_style} wallet" do
          w = BYRON.wallets
          id = create_byron_wallet(wallet_style)
          upd = w.update_passphrase(id, { old_passphrase: 'Secure Passphrase',
                                          new_passphrase: 'Securer Passphrase' })
          expect(upd).to be_correct_and_respond 204
        end

        it "Cannot update_passphrase of #{wallet_style} wallet not knowing old pass" do
          w = BYRON.wallets
          id = create_byron_wallet(wallet_style)
          upd = w.update_passphrase(id, { old_passphrase: 'wrong-passphrase',
                                          new_passphrase: 'Securer Passphrase' })
          expect(upd).to be_correct_and_respond 403
          expect(upd.to_s).to include 'wrong_encryption_passphrase'
        end
      end
    end

    it 'Can see utxo' do
      id = create_byron_wallet
      utxo = BYRON.wallets.utxo(id)
      expect(utxo).to be_correct_and_respond 200
    end

    it 'Can see utxo snapshot' do
      id = create_byron_wallet
      utxo = BYRON.wallets.utxo_snapshot(id)
      expect(utxo).to be_correct_and_respond 200
    end

    describe 'Wallet id' do
      matrix = [
        %w[Byron random],
        %w[Icarus icarus]
      ]
      matrix.each do |m|
        wallet_type = m[0]
        wallet_style = m[1]
        it "I can get #{wallet_type} #{wallet_style} walletid using cardano-addresses" do
          mnemonics = CW.utils.mnemonic_sentence(24)
          wid = create_byron_wallet(wallet_style, 'Wallet - ID', mnemonics)

          # based on root prv key
          root_xsk = CA.prv_key_from_recovery_phrase(mnemonics, wallet_type)
          ca_wid_root_xsk = CA.key_walletid(root_xsk)
          expect(wid).to eq ca_wid_root_xsk

          # based on pub key
          pub_key = CA.key_public(root_xsk)
          ca_wid_pub_key = CA.key_walletid(pub_key)
          expect(wid).to eq ca_wid_pub_key
        end

        it "#{wallet_type} walletid is not based on acct key" do
          mnemonics = CW.utils.mnemonic_sentence(24)
          wid = create_byron_wallet(wallet_style, 'Wallet - ID', mnemonics)

          # based on acct prv key
          root_xsk = CA.prv_key_from_recovery_phrase(mnemonics, wallet_type)
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
  end

  describe CardanoWallet::Byron::Addresses do
    it 'Can list addresses - random', :adp_2211 do
      id = create_byron_wallet
      addresses = BYRON.addresses.list id
      expect(addresses).to be_correct_and_respond 200
      expect(addresses.size).to eq 0

      create_addr = BYRON.addresses.create(id, { passphrase: PASS })
      expect(create_addr).to be_correct_and_respond 201

      addresses = BYRON.addresses.list id
      expect(addresses).to be_correct_and_respond 200

      expect(addresses.size).to eq 1
      expect(addresses.first['id']).to eq create_addr['id']
      expect(addresses.first['derivation_path'][0]).to eq create_addr['derivation_path'].first
      expect(addresses.first['derivation_path'][1]).to eq create_addr['derivation_path'].last
    end

    it 'Can list addresses - icarus' do
      id = create_byron_wallet 'icarus'
      addresses_unused = BYRON.addresses.list id, { state: 'unused' }
      expect(addresses_unused).to be_correct_and_respond 200

      expect(addresses_unused.size).to eq 20
      addresses_unused.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['44H', '1815H', '0H', '0', i.to_s]
      end
    end

    it 'Can list addresses - ledger' do
      id = create_byron_wallet 'ledger'
      addresses_unused = BYRON.addresses.list id, { state: 'unused' }
      expect(addresses_unused).to be_correct_and_respond 200

      expect(addresses_unused.size).to eq 20
      addresses_unused.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['44H', '1815H', '0H', '0', i.to_s]
      end
    end

    it 'Can list addresses - trezor' do
      id = create_byron_wallet 'trezor'
      addresses_unused = BYRON.addresses.list id, { state: 'unused' }
      expect(addresses_unused).to be_correct_and_respond 200

      expect(addresses_unused.size).to eq 20
      addresses_unused.each_with_index do |a, i|
        expect(a['derivation_path']).to eq ['44H', '1815H', '0H', '0', i.to_s]
      end
    end

    it 'Can create address - random' do
      id = create_byron_wallet
      addr = BYRON.addresses.create(id, { passphrase: PASS,
                                          address_index: 2_147_483_648 })
      expect(addr).to be_correct_and_respond 201
      expect(addr['derivation_path']).to eq %w[0H 0H]

      addr_r = BYRON.addresses.create(id, { passphrase: PASS })
      expect(addr_r).to be_correct_and_respond 201
      expect(addr_r['derivation_path'][0]).to eq '0H'
      expect(addr_r['derivation_path'][1]).to end_with 'H'
    end

    it 'I can import address - random' do
      mnemonics = CW.utils.mnemonic_sentence(15)
      derivation_path = '14H/42H'
      id = create_byron_wallet('random', 'Wallet - import', mnemonics)

      addr = cardano_address_get_byron_addr(mnemonics, derivation_path)

      addr_import = BYRON.addresses.import(id, addr)
      expect(addr_import).to be_correct_and_respond 204

      addresses = BYRON.addresses.list id
      expect(addresses).to be_correct_and_respond 200
      expect(addresses.size).to eq 1
      expect(addresses.first['derivation_path']).to eq derivation_path.split('/')
    end

    it 'I cannot import address - icarus' do
      id = create_byron_wallet 'icarus'
      addr = BYRON.addresses.list(id)[0]['id']
      addr_import = BYRON.addresses.import(id, addr)
      expect(addr_import).to be_correct_and_respond 403
      expect(addr_import.to_s).to include 'invalid_wallet_type'
    end

    it 'I cannot import address - ledger' do
      id = create_byron_wallet 'ledger'
      addr = BYRON.addresses.list(id)[0]['id']
      addr_import = BYRON.addresses.import(id, addr)
      expect(addr_import).to be_correct_and_respond 403
      expect(addr_import.to_s).to include 'invalid_wallet_type'
    end

    it 'I cannot import address - trezor' do
      id = create_byron_wallet 'trezor'
      addr = BYRON.addresses.list(id)[0]['id']
      addr_import = BYRON.addresses.import(id, addr)
      expect(addr_import).to be_correct_and_respond 403
      expect(addr_import.to_s).to include 'invalid_wallet_type'
    end
  end

  describe CardanoWallet::Byron::CoinSelections do
    it 'I could trigger random coin selection - if had money' do
      wid = create_byron_wallet 'icarus'
      addresses = BYRON.addresses.list(wid)
      addr_amount = [
        { addresses[0]['id'] => MIN_UTXO_VALUE_PURE_ADA },
        { addresses[1]['id'] => MIN_UTXO_VALUE_PURE_ADA }
      ]

      rnd = BYRON.coin_selections.random wid, addr_amount

      expect(rnd).to be_correct_and_respond 403
      expect(rnd.to_s).to include 'no_utxos_available'
    end
  end

  describe CardanoWallet::Byron::Transactions do
    # Run for random and icarus
    %w[random icarus].each do |style|
      it "I could get a tx if I had proper id - #{style}" do
        wid = create_byron_wallet style
        txs = BYRON.transactions
        g = txs.get(wid, TXID)
        expect(g).to be_correct_and_respond 404
        expect(g.to_s).to include 'no_such_transaction'
      end

      it "Can list transactions - #{style}" do
        id = create_byron_wallet style
        txs = BYRON.transactions

        expect(txs.list(id)).to be_correct_and_respond 200
        expect(txs.list(id, { max_count: 1 })).to be_correct_and_respond 200
        expect(txs.list(id,
                        { start: '2012-09-25T10:15:00Z',
                          end: '2016-11-21T10:15:00Z',
                          order: 'ascending',
                          max_count: 10 }))
          .to be_correct_and_respond 200
        expect(txs.list(id, { order: 'bad_order' })).to be_correct_and_respond 400
        expect(txs.list(id, { max_count: 'bad_count' })).to be_correct_and_respond 400
      end

      it "I could send tx if I had money - #{style}" do
        id = create_byron_wallet style
        target_id = create_byron_wallet 'icarus'
        target_addr = BYRON.addresses.list(target_id)[0]['id']

        tx_sent = BYRON.transactions.create(id, PASS, [{ target_addr => 1_000_000 }])
        expect(tx_sent).to be_correct_and_respond 403
        expect(tx_sent.to_s).to include 'no_utxos_available'
      end

      it "I could estimate fees if I had money - #{style}" do
        id = create_byron_wallet style
        target_id = create_byron_wallet 'icarus'
        target_addr = BYRON.addresses.list(target_id)[0]['id']

        fees = BYRON.transactions.payment_fees(id, [{ target_addr => 1_000_000 }])
        expect(fees).to be_correct_and_respond 403
        expect(fees.to_s).to include 'no_utxos_available'
      end

      it "I could forget transaction - #{style}" do
        id = create_byron_wallet style
        txs = BYRON.transactions
        res = txs.forget(id, TXID)
        expect(res).to be_correct_and_respond 404
      end
    end
  end

  describe CardanoWallet::Byron::Migrations do
    it 'I could create migration plan - icarus' do
      id = create_byron_wallet 'icarus'
      target_id = create_shelley_wallet
      addrs = SHELLEY.addresses.list(target_id).map { |a| a['id'] }

      plan = BYRON.migrations.plan(id, addrs)
      expect(plan).to be_correct_and_respond 403
      expect(plan.to_s).to include 'nothing_to_migrate'
    end

    it 'I could create migration plan - random' do
      id = create_byron_wallet 'random'
      target_id = create_shelley_wallet
      addrs = SHELLEY.addresses.list(target_id).map { |a| a['id'] }

      plan = BYRON.migrations.plan(id, addrs)
      expect(plan).to be_correct_and_respond 403
      expect(plan.to_s).to include 'nothing_to_migrate'
    end

    it 'I could migrate all my funds' do
      id = create_byron_wallet 'random'
      target_wal_id = create_byron_wallet 'icarus'
      addresses = BYRON.addresses.list(target_wal_id).map { |a| a['id'] }
      migr = BYRON.migrations.migrate(id, PASS, addresses)
      expect(migr).to be_correct_and_respond 403
      expect(migr.to_s).to include 'nothing_to_migrate'
    end
  end
end
