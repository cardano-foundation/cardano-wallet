RSpec.describe CardanoWallet::Shared do

  describe CardanoWallet::Shared::Wallets do

    before(:each) do
      teardown
    end


    describe "Create wallets" do
      it "I can create, get and delete wallet from mnemonics getting acc_xpub from cardano-address" do
        w = SHARED.wallets

        m24 = mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")
        script_template = { "cosigners" =>
                              { "cosigner#0" => acc_xpub },
                            "template" =>
                                { "all" =>
                                   [ "cosigner#0",
                                     { "active_from" => 120 }
                                   ]
                                }
                            }

        payload = { mnemonic_sentence: m24,
                    passphrase: PASS,
                    name: "Shared wallet",
                    account_index: acc_ix,
                    payment_script_template: script_template,
                    delegation_script_template: script_template,
                    }

        wallet = w.create(payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = w.get(wid)
        expect(g).to be_correct_and_respond 200

        expect(w.delete(wid)).to be_correct_and_respond 204
      end

      it "I can create, get and delete wallet from pub key getting acc_xpub from cardano-address" do
        w = SHARED.wallets

        m24 = mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/#{acc_ix}")

        payment_script_template = { "cosigners" =>
                                          { "cosigner#0" => acc_xpub },
                                        "template" =>
                                            { "all" =>
                                               [ "cosigner#0",
                                                 { "active_from" => 120 }
                                               ]
                                            }
                                    }

        delegation_script_template = { 'cosigners' =>
                                            { 'cosigner#0' => acc_xpub },
                                        'template' =>
                                            { 'all' =>
                                               [ 'cosigner#0',
                                                 'cosigner#1'
                                               ]
                                            }
                                      }
        payload = { account_public_key: acc_xpub,
                    passphrase: PASS,
                    name: "Shared wallet",
                    account_index: acc_ix,
                    payment_script_template: payment_script_template,
                    payment_script_template: delegation_script_template,
                    }

        wallet = w.create(payload)
        expect(wallet).to be_correct_and_respond 201

        wid = wallet['id']
        g = w.get(wid)
        expect(g).to be_correct_and_respond 200

        expect(w.delete(wid)).to be_correct_and_respond 204
      end

      it "Cannot create wallet with different acc xpub - derived from different mnemonic sentence" do
        w = SHARED.wallets

        m24 = mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub_wrong = cardano_address_get_acc_xpub(mnemonic_sentence(24),
                                                              "1854H/1815H/0H")

        payment_script_template = { "cosigners" =>
                                          { "cosigner#0" => acc_xpub_wrong },
                                        "template" =>
                                            { "all" =>
                                               [ "cosigner#0"
                                               ]
                                            }
                                    }

        delegation_script_template = { "cosigners" =>
                                          { "cosigner#0" => acc_xpub_wrong },
                                        "template" =>
                                            { "all" =>
                                               [ "cosigner#0",
                                                 "cosigner#1"
                                               ]
                                            }
                                      }
        payload = { mnemonic_sentence: m24,
                    passphrase: PASS,
                    name: "Shared wallet",
                    account_index: acc_ix,
                    payment_script_template: payment_script_template,
                    payment_script_template: delegation_script_template,
                    }

        wallet = w.create(payload)
        expect(wallet).to be_correct_and_respond 403

      end

      it "Cannot create wallet with different acc xpub - derived from different acc ix" do
        w = SHARED.wallets

        m24 = mnemonic_sentence(24)
        acc_ix = '0H'
        acc_xpub_wrong = cardano_address_get_acc_xpub(m24, "1854H/1815H/255H")

        payment_script_template = { "cosigners" =>
                                          { "cosigner#0" => acc_xpub_wrong },
                                        "template" =>
                                            { "all" =>
                                               [ "cosigner#0"
                                               ]
                                            }
                                    }

        delegation_script_template = { "cosigners" =>
                                          { "cosigner#0" => acc_xpub_wrong },
                                        "template" =>
                                            { "all" =>
                                               [ "cosigner#0",
                                                 "cosigner#1"
                                               ]
                                            }
                                      }
        payload = { mnemonic_sentence: m24,
                    passphrase: PASS,
                    name: "Shared wallet",
                    account_index: acc_ix,
                    payment_script_template: payment_script_template,
                    payment_script_template: delegation_script_template,
                    }

        wallet = w.create(payload)
        expect(wallet).to be_correct_and_respond 403

      end

      it "I can create pending wallet and update cosigners with acc_xpub from cardano-address" do
        m24 = mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/0H")
        pending_wid = create_pending_shared_wallet(m24, '0H', acc_xpub)

        acc_xpub_upd = cardano_address_get_acc_xpub(mnemonic_sentence(24),
                                                "1854H/1815H/0H")

        update_payment = SHARED.wallets.update_payment_script(pending_wid,
                                                              "cosigner#1",
                                                              acc_xpub_upd)

        expect(update_payment).to be_correct_and_respond 200
        expect(SHARED.wallets.get(pending_wid)).to be_correct_and_respond 200

        update_delegation = SHARED.wallets.update_delegation_script(pending_wid,
                                                                  "cosigner#1",
                                                                  acc_xpub_upd)

        expect(update_delegation).to be_correct_and_respond 200
        expect(SHARED.wallets.get(pending_wid)['state']['status']).to eq 'syncing'

        expect(SHARED.wallets.delete(pending_wid)).to be_correct_and_respond 204
      end

      it "Create / update partially / get / delete" do
        m24 = mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/0H")
        pending_wid = create_pending_shared_wallet(m24, '0H', acc_xpub)

        acc_xpub_upd = cardano_address_get_acc_xpub(mnemonic_sentence(24),
                                                "1854H/1815H/0H")

        update_payment = SHARED.wallets.update_payment_script(pending_wid,
                                                              "cosigner#1",
                                                              acc_xpub_upd)

        expect(update_payment).to be_correct_and_respond 200

        expect(SHARED.wallets.get(pending_wid)).to be_correct_and_respond 200

        expect(SHARED.wallets.delete(pending_wid)).to be_correct_and_respond 204
      end

      it "Cannot update main cosigner" do
        m24 = mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/0H")
        pending_wid = create_pending_shared_wallet(m24, '0H', acc_xpub)
        acc_xpub_upd = cardano_address_get_acc_xpub(mnemonic_sentence(24),
                                                "1854H/1815H/0H")

        update_payment = SHARED.wallets.update_payment_script(pending_wid,
                                                              "cosigner#0",
                                                              acc_xpub_upd)

        expect(update_payment).to be_correct_and_respond 403

        update_delegation = SHARED.wallets.update_delegation_script(pending_wid,
                                                                  "cosigner#0",
                                                                  acc_xpub_upd)

        expect(update_delegation).to be_correct_and_respond 403

        expect(SHARED.wallets.delete(pending_wid)).to be_correct_and_respond 204
      end

      it "Cannot update cosigner with main cosigner's xpub" do
        m24 = mnemonic_sentence(24)
        acc_xpub = cardano_address_get_acc_xpub(m24, "1854H/1815H/0H")
        pending_wid = create_pending_shared_wallet(m24, '0H', acc_xpub)

        update_payment = SHARED.wallets.update_payment_script(pending_wid,
                                                              "cosigner#1",
                                                              acc_xpub)

        expect(update_payment).to be_correct_and_respond 403

        update_delegation = SHARED.wallets.update_delegation_script(pending_wid,
                                                                  "cosigner#1",
                                                                  acc_xpub)

        expect(update_delegation).to be_correct_and_respond 403

        expect(SHARED.wallets.delete(pending_wid)).to be_correct_and_respond 204
      end
    end


  end

end
