RSpec.describe CardanoWallet::Byron do

  describe CardanoWallet::Byron::Wallets do

    after(:each) do
      teardown
    end

    it "I can list byron wallets" do
      l = BYRON.wallets.list
      expect(l).to have_http 200
    end

    it "I could get a wallet" do
      g = BYRON.wallets.get "db66f3d0d796c6aa0ad456a36d5a3ee88d62bd5d"
      expect(g).to have_http 404
    end

    it "I could delete a wallet" do
      g = BYRON.wallets.delete "db66f3d0d796c6aa0ad456a36d5a3ee88d62bd5d"
      expect(g).to have_http 404
    end

    it "I can create, get and delete byron icarus wallet from mnemonics" do
      wallet = BYRON.wallets.create({style: "icarus",
                         name: "Wallet from mnemonic_sentence",
                         passphrase: "Secure Passphrase",
                         mnemonic_sentence: mnemonic_sentence(15),
                         })
      expect(wallet).to have_http 201
      wid = wallet['id']
      expect(BYRON.wallets.get(wid)).to have_http 200
      expect(BYRON.wallets.delete(wid)).to have_http 204
    end

    it "I can create, get and delete byron random wallet from mnemonics" do
      wallet = BYRON.wallets.create({style: "random",
                         name: "Wallet from mnemonic_sentence",
                         passphrase: "Secure Passphrase",
                         mnemonic_sentence: mnemonic_sentence(12),
                         })
      expect(wallet).to have_http 201
      wid = wallet['id']
      expect(BYRON.wallets.get(wid)).to have_http 200
      expect(BYRON.wallets.delete(wid)).to have_http 204
    end

    it "Create param must be hash" do
      w = BYRON.wallets
      expect{w.create("That's bad param")}.to raise_error ArgumentError, "argument should be Hash"
    end

    it "Can update_metadata" do
      w = BYRON.wallets
      id = create_byron_wallet
      expect(w.update_metadata(id,{name: "New wallet name"})).to have_http 200
    end

    it "Can update_passphrase" do
      w = BYRON.wallets
      id = create_byron_wallet
      upd = w.update_passphrase(id,{old_passphrase: "Secure Passphrase",
                                    new_passphrase: "Securer Passphrase"})
      expect(upd).to have_http 204
    end

    it "Can see utxo" do
      id = create_byron_wallet
      utxo = BYRON.wallets.utxo(id)
      expect(utxo).to have_http 200
    end
  end

  describe CardanoWallet::Byron::Addresses do

    after(:each) do
      teardown
    end

    it "Can list addresses - random" do
      id = create_byron_wallet
      addresses = BYRON.addresses.list id
      expect(addresses).to have_http 200
      expect(addresses.size).to eq 0

      BYRON.addresses.create(id, {passphrase: PASS})
      addresses = BYRON.addresses.list id
      expect(addresses).to have_http 200
      expect(addresses.size).to eq 1
    end

    it "Can list addresses - icarus" do
      id = create_byron_wallet "icarus"
      addresses_unused = BYRON.addresses.list id, {state: "unused"}
      expect(addresses_unused).to have_http 200
      expect(addresses_unused.size).to eq 20
    end

    it "Can list addresses - ledger" do
      id = create_byron_wallet "ledger"
      addresses_unused = BYRON.addresses.list id, {state: "unused"}
      expect(addresses_unused).to have_http 200
      expect(addresses_unused.size).to eq 20
    end

    it "Can list addresses - trezor" do
      id = create_byron_wallet "trezor"
      addresses_unused = BYRON.addresses.list id, {state: "unused"}
      expect(addresses_unused).to have_http 200
      expect(addresses_unused.size).to eq 20
    end

    it "Can create address - random" do
      id = create_byron_wallet
      addr = BYRON.addresses.create(id, {passphrase: PASS,
                                         address_index: 2147483648})
      expect(addr).to have_http 201

      addr = BYRON.addresses.create(id, {passphrase: PASS})
      expect(addr).to have_http 201
    end

    it "I could import address - random" do
      id = create_byron_wallet
      addr = BYRON.addresses.create(id, {passphrase: PASS})['id']
      addr_import = BYRON.addresses.import(id, addr)
      expect(addr_import).to have_http 204
    end

    it "I cannot import address - icarus" do
      id = create_byron_wallet "icarus"
      addr = BYRON.addresses.list(id)[0]['id']
      addr_import = BYRON.addresses.import(id, addr)
      expect(addr_import).to include "invalid_wallet_type"
      expect(addr_import).to have_http 403
    end

    it "I cannot import address - ledger" do
      id = create_byron_wallet "ledger"
      addr = BYRON.addresses.list(id)[0]['id']
      addr_import = BYRON.addresses.import(id, addr)
      expect(addr_import).to include "invalid_wallet_type"
      expect(addr_import).to have_http 403
    end

    it "I cannot import address - trezor" do
      id = create_byron_wallet "trezor"
      addr = BYRON.addresses.list(id)[0]['id']
      addr_import = BYRON.addresses.import(id, addr)
      expect(addr_import).to include "invalid_wallet_type"
      expect(addr_import).to have_http 403
    end
  end

  describe CardanoWallet::Byron::CoinSelections do

    after(:each) do
      teardown
    end

    it "I could trigger random coin selection - if had money" do
      wid = create_byron_wallet "icarus"
      addresses = BYRON.addresses.list(wid)
      addr_amount = [
         { addresses[0]['id'] => 123 },
         { addresses[1]['id'] => 456 }
        ]

      rnd = BYRON.coin_selections.random wid, addr_amount

      expect(rnd).to include "not_enough_money"
      expect(rnd).to have_http 403
    end

    it "ArgumentError on bad argument address_amount" do
      wid = create_byron_wallet
      p =[[{addr1: 1, addr2: 2}], "addr:123", 123]
      cs = BYRON.coin_selections
      expect{ cs.random(wid, p[0]) }.to raise_error ArgumentError,
            "argument should be Array of single Hashes"

      expect{ cs.random(wid, p[1]) }.to raise_error ArgumentError,
            "argument should be Array"

      expect{ cs.random(wid, p[2]) }.to raise_error ArgumentError,
            "argument should be Array"
    end
  end

  describe CardanoWallet::Byron::Transactions do
    after(:each) do
      teardown
    end

    # Run for random and icarus
    ["random", "icarus"].each do |style|

      it "I could get a tx if I had proper id - #{style}" do
        wid = create_byron_wallet style
        txs = BYRON.transactions
        expect(txs.get(wid, TXID)).to include "no_such_transaction"
        expect(txs.get(wid, TXID)).to have_http 404
      end

      it "Can list transactions - #{style}" do
        id = create_byron_wallet style
        txs = BYRON.transactions

        expect(txs.list(id)).to have_http 200
        expect(txs.list(id,
                        {start: "2012-09-25T10:15:00Z",
                         end: "2016-11-21T10:15:00Z",
                         order: "ascending"}).code).
                        to eq 200
        expect(txs.list(id, {order: "bad_order"})).to have_http 400
      end

      it "I could send tx if I had money - #{style}" do
        id = create_byron_wallet style
        target_id = create_byron_wallet "icarus"
        target_addr = BYRON.addresses.list(target_id)[0]['id']

        tx_sent = BYRON.transactions.create(id, PASS, [{target_addr => 1000000}])
        expect(tx_sent).to include "not_enough_money"
        expect(tx_sent).to have_http 403
      end

      it "I could estimate fees if I had money - #{style}" do
        id = create_byron_wallet style
        target_id = create_byron_wallet "icarus"
        target_addr = BYRON.addresses.list(target_id)[0]['id']

        fees = BYRON.transactions.payment_fees(id, [{target_addr => 1000000}])
        expect(fees).to include "not_enough_money"
        expect(fees).to have_http 403
      end

      it "I could forget transaction - #{style}" do
        id = create_byron_wallet style
        txs = BYRON.transactions
        res = txs.forget(id, TXID)
        expect(res).to have_http 404
      end
    end
  end

  describe CardanoWallet::Byron::Migrations do
    after(:each) do
      teardown
    end

    it "I could calculate migration cost" do
      id = create_byron_wallet "icarus"
      cost = BYRON.migrations.cost(id)
      expect(cost).to have_http 501
    end

    it "I could migrate all my funds" do
      id = create_byron_wallet "random"
      target_wal_id = create_byron_wallet "icarus"
      addresses = BYRON.addresses.list(target_wal_id).map{ |a| a['id'] }
      migr = BYRON.migrations.migrate(id, PASS, addresses)
      expect(migr).to have_http 501
    end
  end
end
