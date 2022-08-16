# coding: utf-8
RSpec.describe "Light-mode E2E tests", :light do

  describe "Syncing" do
    it "I can restore empty wallet" do
      wid = create_shelley_wallet
      wait_for_shelley_wallet_to_sync(wid)
      wallet = SHELLEY.wallets.get(wid)
      txs = SHELLEY.transactions.list(wid)
      expect(wallet['balance']['total']['quantity']).to eq 0
      expect(txs.size).to eq 0
    end

    it "I can restore wallet with funds and I see ADA balance", :light_sync do
      wid = create_fixture_wallet(:shelley_light)
      wait_for_shelley_wallet_to_sync(wid)
      wallet = SHELLEY.wallets.get(wid)
      txs = SHELLEY.transactions.list(wid)
      expect(wallet['balance']['total']['quantity']).to eq 500000000
      expect(txs.size).to eq 1
    end

    describe "Network" do
      it "Can get network information" do
        res = NETWORK.information
        expect(res).to be_correct_and_respond 200
        expect(res['network_info']['protocol_magic']).to eq get_protocol_magic
        expect(res['network_info']['network_id']).to eq 'testnet'
        expect(res['wallet_mode']).to eq 'light'
      end
    end
  end
end
