# coding: utf-8
RSpec.describe "Light-mode E2E tests", :light do

  after(:each) do
    teardown
  end

  describe "Syncing" do
    before(:all) do
      @wid_light = create_fixture_wallet(:shelley_light)
      wait_for_shelley_wallet_to_sync(@wid_light)
    end

    it "I can restore empty wallet" do
      wid = create_shelley_wallet
      wait_for_shelley_wallet_to_sync(wid)
      wallet = SHELLEY.wallets.get(wid)
      txs = SHELLEY.transactions.list(wid)
      expect(wallet['balance']['total']['quantity']).to eq 0
      expect(txs.size).to eq 0
    end

    it "I can restore wallet with funds and I see ADA and assets balance", :light_sync do
      wallet = SHELLEY.wallets.get(@wid_light)
      expect(wallet['balance']['total']['quantity']).to be > 0
      expect(wallet['assets']['total'].first['quantity']).to be > 0
    end
  end

  describe "Network" do
    it "Can get network information" do
      res = NETWORK.information
      expect(res).to be_correct_and_respond 200
      expect(res['network_info']['protocol_magic']).to eq get_protocol_magic(CONTEXT.env)
      expect(res['network_info']['network_id']).to eq 'testnet'
      expect(res['wallet_mode']).to eq 'light'
    end
  end
end
