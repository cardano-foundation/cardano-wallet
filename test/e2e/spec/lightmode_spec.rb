# coding: utf-8
RSpec.describe "Light-mode E2E tests", :light do

  describe "Syncing" do
    it "I can restore empty wallet" do
      wid = create_shelley_wallet
      wait_for_shelley_wallet_to_sync(wid)
      wallet = SHELLEY.wallets.get(wid)
      expect(wallet['balance']['total']['quantity']).to eq 0
    end

    it "I can restore wallet with funds and I see ADA balance" do
      wid = create_fixture_wallet(:shelley)
      wait_for_shelley_wallet_to_sync(wid)
      wallet = SHELLEY.wallets.get(wid)
      expect(wallet['balance']['total']['quantity']).to be > 0
    end
  end
end
