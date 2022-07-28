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
  end
end
