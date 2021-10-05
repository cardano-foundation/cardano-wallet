RSpec.describe CardanoWallet::Misc do

  describe CardanoWallet::Misc::Network do

    it "Can get network information" do
      res = NETWORK.information
      expect(res).to be_correct_and_respond 200
    end

    it "Can check network clock offset" do
      res = NETWORK.clock
      expect(res).to be_correct_and_respond 200
    end

    it "Can check network parameters" do
      network_params = ["slot_length",
                        "decentralization_level",
                        "maximum_token_bundle_size",
                        "genesis_block_hash",
                        "blockchain_start_time",
                        "desired_pool_number",
                        "execution_unit_prices",
                        "eras",
                        "minimum_collateral_percentage",
                        "active_slot_coefficient",
                        "security_parameter",
                        "minimum_utxo_value",
                        "maximum_collateral_input_count"]
      res = NETWORK.parameters
      expect(res).to be_correct_and_respond 200
      network_params.each do |p|
        expect(res.to_s).to include p
      end
    end
  end

  describe CardanoWallet::Misc::Utils do

    describe "SMASH health" do
      it "SMASH health - unreachable" do
        r = UTILS.smash_health({ url: "http://onet.pl" })
        expect(r).to be_correct_and_respond 200
        expect(r.to_s).to include "unreachable"
      end

      it "SMASH health - bad url" do
        r = UTILS.smash_health({ url: "dsds" })
        expect(r).to be_correct_and_respond 400
        expect(r.to_s).to include "bad_request"
      end
    end

    it "Inspect invalid address" do
      addr = "addr"
      res = UTILS.addresses addr
      expect(res).to be_correct_and_respond 400
      expect(res.to_s).to include "bad_request"
    end

    it "Inspect Shelley payment address" do
      addr = "addr1qqlgm2dh3vpv07cjfcyuu6vhaqhf8998qcx6s8ucpkly6f8l0dw5r75vk42mv3ykq8vyjeaanvpytg79xqzymqy5acmqej0mk7"
      res = UTILS.addresses addr

      expect(res).to be_correct_and_respond 200
      expect(res['address_style']).to eq "Shelley"
      expect(res['stake_reference']).to eq "by value"
      expect(res['stake_key_hash']).to eq "ff7b5d41fa8cb555b6449601d84967bd9b0245a3c530044d8094ee36"
      expect(res['spending_key_hash']).to eq "3e8da9b78b02c7fb124e09ce6997e82e9394a7060da81f980dbe4d24"
      expect(res['network_tag']).to eq 0
    end

    it "Inspect Shelley stake address" do
      addr = "stake_test1uzws33ghf8kugc8ea8p7h8mr7dcsl6ggw7tfy479y9t0d4qp48dkq"
      res = UTILS.addresses addr

      expect(res).to be_correct_and_respond 200
      expect(res['address_style']).to eq "Shelley"
      expect(res['stake_reference']).to eq "by value"
      expect(res['stake_key_hash']).to eq "9d08c51749edc460f9e9c3eb9f63f3710fe90877969257c52156f6d4"
      expect(res['network_tag']).to eq 0
    end

    it "Inspect Byron Random address" do
      addr = "37btjrVyb4KEzz6YprjHfqz3DS4JvoDpAf3QWLABzQ7uzMEk7g3PD2AwL1SbYWekneuRFkyTipbyKMZyEMed5LroZtQAvA2LqcWmJuwaqt6oJLbssS"
      res = UTILS.addresses addr

      expect(res).to be_correct_and_respond 200
      expect(res['address_style']).to eq "Byron"
      expect(res['stake_reference']).to eq "none"
      expect(res['address_root']).to eq "c23a0f86c7bc977f0dee4721c9850467047a0e6acd928a991b5cbba8"
      expect(res['derivation_path']).to eq "581c6a6589ca57730b33d1bb316c13a76d7794a11ba2d077724bdfb51b45"
      expect(res['network_tag']).to eq 1097911063
    end

    it "Inspect Byron Icarus address" do
      addr = "2cWKMJemoBajQcoTotf4xYba7fV7Ztx7AvbnzvaQY6PbezPWM6DtJD6Df2bVejBCpykmt"
      res = UTILS.addresses addr

      expect(res).to be_correct_and_respond 200
      expect(res['address_style']).to eq "Icarus"
      expect(res['stake_reference']).to eq "none"
      expect(res['address_root']).to eq "88940c753ee50d556ecaefadd0d2fee9fabacf4366a7d4a8cdfa2b64"
      expect(res['network_tag']).to eq 1097911063
    end

    describe "Construct addresses" do
      it "Enterprise script address - signature" do
        script = {
                  "payment": "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq"
                 }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "addr_test1wrrqr8fmk4ulc7pgycd96fuqcg40e5ecuway0ypc2tsnteqqu6qs0"
      end

      it "Enterprise script address - any" do
        script = {
                  "payment": {
                      "any": [
                          "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                          "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"
                          ]
                      }
                  }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "addr_test1wrjtlgneqelxxlckcsgrkd7rd6ycrgegu5th24x0f058gmqvc8s20"
      end

      it "Enterprise script address - all" do
        script = {
                  "payment": {
                      "all": [
                          "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                          "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp"
                          ]
                      }
                  }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "addr_test1wpq0ghwy73wapjcdwqxm6ytwe66j8eccsmn9jptshrjerashp7y82"
      end

      it "Enterprise script address - some" do
        script = {
                  "payment": {
                      "some": {
                          "from": [
                              "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                              "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                              "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                              ],
                           "at_least": 2
                           }
                   }
                 }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "addr_test1wqqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84agpc0nuz"
      end

      it "Reward account script address - any" do
        script = {
                  "stake": {
                      "any": [
                          "stake_shared_vkh1nqc00hvlc6cq0sfhretk0rmzw8dywmusp8retuqnnxzajtzhjg5",
                          "stake_shared_vkh1nac0awgfa4zjsh4elnjmsscz0huhss8q2g0x3n7m539mwaa5m7s"
                          ]
                      }
                  }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "stake_test17qshpfjkgh98wumvnn9y3yfhevllp4y04u6y84q3flxcv9s2kvrnx"
      end

      it "Delegating script address - any" do
        script = {
                    "payment": {
                        "some": {
                            "from": [
                                "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                                "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                                "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                                ],
                             "at_least": 2
                             }
                        },
                    "stake": {
                        "any": [
                            "script_vkh1yf07000d4ml3ywd3d439kmwp07xzgv6p35cwx8h605jfx0dtd4a",
                            "script_vkh1mwlngj4fcwegw53tdmyemfupen2758xwvudmcz9ap8cnqk7jmh4"
                            ]
                        }
                  }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "addr_test1wqqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84agpc0nuz"
      end

      it "Enterprise pub key address" do
        script = {
                  "payment": "addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j"
                 }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "addr_test1vpqthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfgdmxm9u"
      end

      it "Reward account pub key address" do
        script = {
                  "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
                 }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "stake_test1uq6pmlvyl3wn4ca6807e26gy2gek9hqu0gastzh5tk0xx0gdfvj8f"
      end

      it "Delegating address with both pub key credentials" do
        script = {
                  "payment": "addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j",
                  "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
                 }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "addr_test1qpqthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wff5rh7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7ser7yl4"
      end

      it "Delegating address - payment from script, stake from key" do
        script = {
                  "payment": {
                      "some": {
                          "from": [
                              "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                              "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                              "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                              ],
                           "at_least": 2
                           }
                       },
                  "stake": "stake_vk16apaenn9ut6s40lcw3l8v68xawlrlq20z2966uzcx8jmv2q9uy7qau558d"
                 }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "addr_test1zqqmnmwuh85e0fxaggl6ac2hfeqncg76gsr0ld8qdjd84af5rh7cflza8t3m5wlaj45sg53nvtwpc73mqk90ghv7vv7sq4jdnx"
      end

      it "Delegating address - payment from key, stake from script" do
        script = {
                  "payment": "addr_vk1lqglg77z6kajsdz4739q22c0zm0yhuy567z6xk2vc0z5ucjtkwpschzd2j",
                  "stake": {
                      "some": {
                          "from": [
                              "addr_shared_vkh1zxt0uvrza94h3hv4jpv0ttddgnwkvdgeyq8jf9w30mcs6y8w3nq",
                              "addr_shared_vkh1y3zl4nqgm96ankt96dsdhc86vd5geny0wr7hu8cpzdfcqskq2cp",
                              "addr_shared_vkh175wsm9ckhm3snwcsn72543yguxeuqm7v9r6kl6gx57h8gdydcd9"
                              ],
                           "at_least": 2
                           }
                      }
                   }
        res = UTILS.post_address(script)
        expect(res).to be_correct_and_respond 202
        expect(res.to_s).to include "addr_test1ypqthemrg5kczwfjjnahwt65elhrl95e9hcgufnajtp6wfgph8kaew0fj7jd6s3l4ms4wnjp8s3a53qxl76wqmy60t6snk9ahl"
      end

    end
  end

  describe CardanoWallet::Misc::Proxy do
    it "Malformed payload when tx is not binary" do
      pending "ADP-1145 - DecoderErrorDeserialiseFailure error message from the API /proxy/transactions"
      no_binary_blob = "test"
      res = PROXY.submit_external_transaction(no_binary_blob)
      expect(res).to be_correct_and_respond 400
      expect(res.to_s).to include "malformed_tx_payload"
    end
  end

  describe CardanoWallet::Misc::Settings do

    after(:all) do
      SETTINGS.update({ :pool_metadata_source => "none" })
    end

    matrix = { "direct" => "no_smash_configured",
              "https://smash.pl" => "unreachable",
              "none" => "no_smash_configured" }

    matrix.each do |strategy, smash_health_response|
      it "I can read and update settings to #{strategy} and verify SMASH health = #{smash_health_response}" do
        s = SETTINGS.update({ :pool_metadata_source => strategy })
        expect(s).to be_correct_and_respond 204

        g = SETTINGS.get
        expect(g['pool_metadata_source']).to eq strategy
        expect(g).to be_correct_and_respond 200

        # check smash health
        r = UTILS.smash_health
        expect(r).to be_correct_and_respond 200
        expect(r.to_s).to include smash_health_response
      end
    end
  end

end
