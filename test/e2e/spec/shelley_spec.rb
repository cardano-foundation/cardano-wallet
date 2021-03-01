RSpec.describe CardanoWallet::Shelley do

  describe CardanoWallet::Shelley::Wallets do

    before(:each) do
      teardown
    end

    it "I can list wallets" do
      l = SHELLEY.wallets.list
      expect(l.code).to eq 200
      expect(l.size).to eq 0

      create_shelley_wallet
      l = SHELLEY.wallets.list
      expect(l.code).to eq 200
      expect(l.size).to eq 1
    end

    it "When wallet does not exist it gives 404" do
      wid = create_shelley_wallet
      SHELLEY.wallets.delete wid
      g = SHELLEY.wallets.get wid
      expect(g.code).to eq 404

      d = SHELLEY.wallets.delete wid
      expect(d.code).to eq 404
    end

    describe "Create wallets" do
      it "Create param must be hash" do
        w = SHELLEY.wallets
        expect{w.create("That's bad param")}.to raise_error ArgumentError, "argument should be Hash"
      end

      it "I can create, get and delete wallet from mnemonics" do
        w = SHELLEY.wallets
        wallet = w.create({name: "Wallet from mnemonic_sentence",
                           passphrase: "Secure Passphrase",
                           mnemonic_sentence: mnemonic_sentence("15"),
                           })
        expect(wallet.code).to eq 201
        wid = wallet['id']
        expect(w.get(wid).code).to eq 200
        expect(w.delete(wid).code).to eq 204
      end

      it "I can create, get and delete wallet from mnemonics / second factor" do
        w = SHELLEY.wallets
        wallet = w.create({name: "Wallet from mnemonic_sentence",
                           passphrase: "Secure Passphrase",
                           mnemonic_sentence: mnemonic_sentence("15"),
                           mnemonic_second_factor: mnemonic_sentence("12")
                           })
        expect(wallet.code).to eq 201
        wid = wallet['id']
        expect(w.get(wid).code).to eq 200
        expect(w.delete(wid).code).to eq 204
      end

      it "I can set address pool gap" do
        pool_gap = 55
        w = SHELLEY.wallets
        wallet = w.create({name: "Wallet from mnemonic_sentence",
                           passphrase: "Secure Passphrase",
                           mnemonic_sentence: mnemonic_sentence("15"),
                           address_pool_gap: pool_gap
                           })
        expect(wallet.code).to eq 201
        addr = SHELLEY.addresses.list(wallet['id'])
        expect(addr.code).to eq 200
        expect(addr.size).to eq pool_gap
      end

      it "I can create, get and delete wallet from pub key" do
        w = SHELLEY.wallets
        wallet = w.create({name: "Wallet from pub key",
                           account_public_key: "b47546e661b6c1791452d003d375756dde6cac2250093ce4630f16b9b9c0ac87411337bda4d5bc0216462480b809824ffb48f17e08d95ab9f1b91d391e48e66b",
                           address_pool_gap: 20,
                           })
        expect(wallet.code).to eq 201
        wid = wallet['id']
        expect(w.get(wid).code).to eq 200
        expect(w.delete(wid).code).to eq 204
      end
    end

    describe "Update wallet" do
      it "Can update_metadata" do
        new_name = "New wallet name"
        w = SHELLEY.wallets
        id = create_shelley_wallet
        expect(w.update_metadata(id, {name: new_name}).code).to eq 200
        expect(w.get(id)['name']).to eq new_name
      end

      it "Can update_passphrase" do
        w = SHELLEY.wallets
        id = create_shelley_wallet
        upd = w.update_passphrase(id,{old_passphrase: "Secure Passphrase",
                                      new_passphrase: "Securer Passphrase"})
        expect(upd.code).to eq 204
      end
    end

    it "Can see utxo" do
      id = create_shelley_wallet
      utxo = SHELLEY.wallets.utxo(id)
      expect(utxo.code).to eq 200
    end
  end

  describe CardanoWallet::Shelley::Addresses do

    after(:each) do
      teardown
    end

    it "Can list addresses" do
      id = create_shelley_wallet
      shelley_addr = CardanoWallet.new.shelley.addresses
      addresses = shelley_addr.list id
      expect(addresses.code).to eq 200
      expect(addresses.size).to eq 20

      addresses_unused = shelley_addr.list id, {state: "used"}
      expect(addresses_unused.code).to eq 200
      expect(addresses_unused.size).to eq 0

      addresses_unused = shelley_addr.list id, {state: "unused"}
      expect(addresses_unused.code).to eq 200
      expect(addresses_unused.size).to eq 20
    end
  end

  describe CardanoWallet::Shelley::CoinSelections do

    after(:each) do
      teardown
    end

    it "I could trigger random coin selection - if had money" do
      wid = create_shelley_wallet
      addresses = SHELLEY.addresses.list(wid)
      addr_amount = [
         { addresses[0]['id'] => 123 },
         { addresses[1]['id'] => 456 }
        ]

      rnd = SHELLEY.coin_selections.random wid, addr_amount
      expect(rnd).to include "not_enough_money"
      expect(rnd.code).to eq 403
    end

    it "ArgumentError on bad argument address_amount" do
      wid = create_shelley_wallet
      p =[[{addr1: 1, addr2: 2}], "addr:123", 123]
      cs = SHELLEY.coin_selections
      expect{ cs.random(wid, p[0]) }.to raise_error ArgumentError,
            "argument should be Array of single Hashes"

      expect{ cs.random(wid, p[1]) }.to raise_error ArgumentError,
            "argument should be Array"

      expect{ cs.random(wid, p[2]) }.to raise_error ArgumentError,
            "argument should be Array"
    end
  end

  describe CardanoWallet::Shelley::Transactions do

    after(:each) do
      teardown
    end

    it "I could get a tx if I had proper id" do
      wid = create_shelley_wallet
      txs = SHELLEY.transactions
      expect(txs.get(wid, TXID)).to include "no_such_transaction"
      expect(txs.get(wid, TXID).code).to eq 404
    end

    it "Can list transactions" do
      id = create_shelley_wallet
      txs = SHELLEY.transactions

      expect(txs.list(id).code).to eq 200
      expect(txs.list(id,
                      {start: "2012-09-25T10:15:00Z",
                       end: "2016-11-21T10:15:00Z",
                       order: "ascending"}).code).
                      to eq 200
      expect(txs.list(id, {order: "bad_order"}).code).to eq 400

    end

    it "I could create transaction - if I had money" do
      id = create_shelley_wallet
      target_id = create_shelley_wallet
      address = SHELLEY.addresses.list(target_id)[0]['id']
      txs = SHELLEY.transactions
      amt = [{address => 1000000}]

      tx_sent = txs.create(id, PASS, amt)
      expect(tx_sent).to include "not_enough_money"
      expect(tx_sent.code).to eq 403
    end

    it "I could create transaction using rewards - if I had money" do
      id = create_shelley_wallet
      target_id = create_shelley_wallet
      address = SHELLEY.addresses.list(target_id)[0]['id']
      txs = SHELLEY.transactions
      amt = [{address => 1000000}]

      tx_sent = txs.create(id, PASS, amt, 'self')
      expect(tx_sent).to include "not_enough_money"
      expect(tx_sent.code).to eq 403
    end

    it "I could estimate transaction fee - if I had money" do
      id = create_shelley_wallet
      target_id = create_shelley_wallet
      address = SHELLEY.addresses.list(target_id)[0]['id']
      amt = [{address => 1000000}]

      txs = SHELLEY.transactions

      fees = txs.payment_fees(id, amt)
      expect(fees).to include "not_enough_money"
      expect(fees.code).to eq 403

      fees = txs.payment_fees(id, amt, 'self')
      expect(fees).to include "not_enough_money"
      expect(fees.code).to eq 403

      metadata = { "0"=>{ "string"=>"cardano" },
                   "1"=>{ "int"=>14 },
                   "2"=>{ "bytes"=>"2512a00e9653fe49a44a5886202e24d77eeb998f" },
                   "3"=>{ "list"=>[ { "int"=>14 }, { "int"=>42 }, { "string"=>"1337" } ] },
                   "4"=>{ "map"=>[ { "k"=>{ "string"=>"key" }, "v"=>{ "string"=>"value" } },
                                   { "k"=>{ "int"=>14 }, "v"=>{ "int"=>42 } } ] } }

      fees = txs.payment_fees(id, amt, 'self', metadata)
      expect(fees).to include "not_enough_money"
      expect(fees.code).to eq 403
    end

    it "I could forget transaction" do
      id = create_shelley_wallet
      txs = SHELLEY.transactions
      res = txs.forget(id, TXID)
      expect(res.code).to eq 404
    end
  end

  describe CardanoWallet::Shelley::StakePools do

    after(:each) do
      teardown
    end

    # it "Can join all pools from cardano_cli", :nightly => true do
    #   id = create_fixture_shelley_wallet
    #   wait_for_shelley_wallet_to_sync id
    #
    #   pools = File.read("spec/pools.txt")
    #   pools.split.each do |pool_id|
    #     puts "Tryin to join pool: #{pool_id}"
    #     join = SHELLEY.stake_pools.join(pool_id, id, PASS)
    #     join_tx_id = join['id']
    #     puts join
    #     puts join_tx_id
    #     expect(join.code).to eq 202
    #     eventually "Checking if join transaction tx is in_ledger" do
    #       tx = SHELLEY.transactions.get(id, join_tx_id)
    #       tx['status'] == "in_ledger"
    #     end
    #   end
    #
    # end
    describe "Stake Pools GC Maintenance" do
      matrix = [{"direct" => "not_applicable"},
                {"none" => "not_applicable"},
                {"https://smash.cardano-testnet.iohkdev.io" => "has_run"}]
      matrix.each do |tc|
        it "GC metadata maintenance action on metadata source #{tc}" do
          settings = CardanoWallet.new.misc.settings
          pools = SHELLEY.stake_pools

          s = settings.update({:pool_metadata_source => tc.keys.first})
          expect(s.code).to eq 204

          t = pools.trigger_maintenance_actions({maintenance_action: "gc_stake_pools"})
          expect(t.code).to eq 204

          eventually "Maintenance action has status = #{tc.values.first}" do
            r = pools.view_maintenance_actions
            (r.code == 200) && (r.to_s.include? tc.values.first)
          end
        end
      end
    end
    it "I could quit stake pool - if I was delegating" do
      id = create_shelley_wallet

      pools = SHELLEY.stake_pools
      quit = pools.quit(id, PASS)
      expect(quit).to include "not_delegating_to"
      expect(quit.code).to eq 403
    end

  end

  describe CardanoWallet::Shelley::Migrations do
    after(:each) do
      teardown
    end

    it "I could calculate migration cost" do
      id = create_shelley_wallet
      cost = SHELLEY.migrations.cost(id)
      expect(cost).to include "not_implemented"
      expect(cost.code).to eq 501
    end

    it "I could migrate all my funds" do
      id = create_shelley_wallet
      target_id = create_shelley_wallet
      addrs = SHELLEY.addresses.list(target_id).map{ |a| a['id'] }
      migr = SHELLEY.migrations.migrate(id, PASS, addrs)
      expect(migr).to include "not_implemented"
      expect(migr.code).to eq 501
    end
  end

  describe CardanoWallet::Shelley::Keys do
    after(:each) do
      teardown
    end

    it "Get signed metadata" do
      wid = create_shelley_wallet
      ["utxo_internal", "utxo_external", "mutable_account", "multisig_script"].each do |role|
        id = [*0..100000].sample
        res = SHELLEY.keys.sign_metadata(wid,
                                        role,
                                        id,
                                        "Secure Passphrase",
                                        { "0"=>{ "string"=>"cardano" } })
        puts "#{wid}/#{role}/#{id}"
        expect(res.code).to eq 200
      end
    end

    it "Get public key" do
      wid = create_shelley_wallet
      ["utxo_internal", "utxo_external", "mutable_account", "multisig_script"].each do |role|
        id = [*0..100000].sample
        res = SHELLEY.keys.get_public_key(wid, role, id)
        puts "#{wid}/#{role}/#{id}"
        expect(res.code).to eq 200
      end
    end

    it "Create account public key - extended = true" do
      wid = create_shelley_wallet
      ["0H", "1H", "2147483647H", "44H"].each do |index|
        res = SHELLEY.keys.create_acc_public_key(wid, index, PASS, extended = true)
        expect(res).to include "acc"
        expect(res.code).to eq 202
      end
    end

    it "Create account public key - extended = false" do
      wid = create_shelley_wallet
      ["0H", "1H", "2147483647H", "44H"].each do |index|
        res = SHELLEY.keys.create_acc_public_key(wid, index, PASS, extended = false)
        expect(res).to include "acc"
        expect(res.code).to eq 202
      end
    end

  end

end
