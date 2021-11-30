require "bundler/setup"
require "cardano_wallet"
require "base64"
require "blake2b"
require "mustache"
require "cbor"
require_relative "../env"
require_relative "../helpers/utils"
require_relative "../helpers/matchers"

include Helpers::Utils

RSpec.configure do |config|
  # Enable flags like --only-failures and --next-failure
  config.example_status_persistence_file_path = ".rspec_status"

  # Disable RSpec exposing methods globally on `Module` and `main`
  config.disable_monkey_patching!

  config.expect_with :rspec do |c|
    c.syntax = :expect
  end
end


# Helpers

##
# timeout in seconds for custom verifications
TIMEOUT = 600

##
# Intit cardano-wallet wrapper with timeout for getting the response back
CW = CardanoWallet.new({ timeout: TIMEOUT, port: ENV['WALLET_PORT'].to_i })
BYRON = CW.byron
SHELLEY = CW.shelley
SHARED = CW.shared
SETTINGS = CW.misc.settings
UTILS = CW.misc.utils
NETWORK = CW.misc.network
PROXY = CW.misc.proxy

##
# default passphrase for wallets
PASS = "Secure Passphrase"

##
# Artificial, non-existing id's
TXID = "1acf9c0f504746cbd102b49ffaf16dcafd14c0a2f1bbb23af265fbe0a04951cc"
SPID = "feea59bc6664572e631e9adfee77142cb51264156debf2e52970cc00"
SPID_BECH32 = "pool1v7g9ays8h668d74xjvln9xuh9adzh6xz0v0hvcd3xukpck5z56d"

# exemplary metadata
METADATA = { "0" => { "string" => "cardano" },
             "1" => { "int" => 14 },
             "2" => { "bytes" => "2512a00e9653fe49a44a5886202e24d77eeb998f" },
             "3" => { "list" => [ { "int" => 14 }, { "int" => 42 }, { "string" => "1337" } ] },
             "4" => { "map" => [ { "k" => { "string" => "key" }, "v" => { "string" => "value" } },
                             { "k" => { "int" => 14 }, "v" => { "int" => 42 } } ] } }

# Testnet assets with metadata from mock server https://metadata.cardano-testnet.iohkdev.io/
ASSETS = [ { "policy_id" => "ee1ce9d7560f48a4ba3867037dbec2d8fed776d94dd6b00a35309073",
             "asset_name" => "",
             "fingerprint" => "asset1s3yhz885gnyu2wcpz5h275u37hw3axz3c9sfqu",
             "metadata" => { "name" => "SadCoin",
                            "description" => "Coin with no asset name",
                            "url" => "https://sad.io",
                            "ticker" => "SAD",
                            "logo" => "iVBORw0KGgoAAAANSUhEUgAAABkAAAAeCAYAAADZ7LXbAAAACXBIWXMAAA7EAAAOxAGVKw4bAAACbUlEQVRIie3Vy0tUURzA8e855965c8lXUhlhEQVBSEmQRAURQbSIEqFl4N6/oHYtAhdtonatK8hVBCERZC+0jbZpIRVkIeagTJrO3Nd5tBhDMHOcGiHCA2dxHvDh9zs/fkc45xwbPORGA5tI/RFdGCL9MgAm/mNEVKuuaHA3OW+RlDb8zjt4O07VjFRPV8NBZC5PGMxj3/YQv7uGs7p+iJ5+ipgfIZr7hnWSXBjgT98iHr6IS+fqg7h0Dl8ZQpmQFKdJSmWkkuSj10TD3WCzv0f89m6S8BjWQehbVDpPWiojsASlEeLxG3WIJFtANneQei3EqpnMeWRxgtMahYGP/dhoqiry2+rKJh9i3l8l2KIRUlVQazDlRXTpOzIr43uQ7LlCvrO/9kjisT7Ehz6CBgtCki4sEC+ALpdQQUC+qQmXC3EO3NQAsHaP/QVx1mBnh5BKYpOYON2L6npJ/sw4svMRacmCc+TyOQwKGX/CRl9rQ4SQyPZeFqM27L7bhCcHUY37AVCtR7EtZ8EZhLN4vkIKhy1N1Ibo4ijq83UavAl04QmIFVekB1aDNQhnQFBZ14KABauRaFThHrrwbPmkPImYeQw6A5OBNRjnIxsPrIl4KzdUcwep9SFL8JVHNnqJeFcvyBCm7hJQBKPBZJWH334eGe5cE1m1hKM3l8nP3kcICVLiEEuXLfycQKpBnnhRtWmuWsLBkZtEucNYa8BkCJMiTFrJ/RLgHJjWc+vqyqsiMthGePo5SWsP2ohKWpamdZBqQbz1AvnjD6oCsI7/RM+8whTHljf8RrzWLlTLoXUB60LqMf6NP34T+T+RH/HOKLJ+ho1iAAAAAElFTkSuQmCC"
                            }
           },
           { "policy_id" => "919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149",
             "asset_name" => asset_name("HappyCoin"),
             "fingerprint" => "asset19mwamgpre24at3z34v2e5achszlhhqght9djqp",
             "metadata" => { "name" => "HappyCoin",
                            "description" => "Coin with asset name - and everyone is happy!!!",
                            "url" => "https://happy.io",
                            "decimals" => 6,
                            "ticker" => "HAPP",
                            "logo" => "iVBORw0KGgoAAAANSUhEUgAAABkAAAAeCAYAAADZ7LXbAAAACXBIWXMAAA7EAAAOxAGVKw4bAAACbUlEQVRIie3Vy0tUURzA8e855965c8lXUhlhEQVBSEmQRAURQbSIEqFl4N6/oHYtAhdtonatK8hVBCERZC+0jbZpIRVkIeagTJrO3Nd5tBhDMHOcGiHCA2dxHvDh9zs/fkc45xwbPORGA5tI/RFdGCL9MgAm/mNEVKuuaHA3OW+RlDb8zjt4O07VjFRPV8NBZC5PGMxj3/YQv7uGs7p+iJ5+ipgfIZr7hnWSXBjgT98iHr6IS+fqg7h0Dl8ZQpmQFKdJSmWkkuSj10TD3WCzv0f89m6S8BjWQehbVDpPWiojsASlEeLxG3WIJFtANneQei3EqpnMeWRxgtMahYGP/dhoqiry2+rKJh9i3l8l2KIRUlVQazDlRXTpOzIr43uQ7LlCvrO/9kjisT7Ehz6CBgtCki4sEC+ALpdQQUC+qQmXC3EO3NQAsHaP/QVx1mBnh5BKYpOYON2L6npJ/sw4svMRacmCc+TyOQwKGX/CRl9rQ4SQyPZeFqM27L7bhCcHUY37AVCtR7EtZ8EZhLN4vkIKhy1N1Ibo4ijq83UavAl04QmIFVekB1aDNQhnQFBZ14KABauRaFThHrrwbPmkPImYeQw6A5OBNRjnIxsPrIl4KzdUcwep9SFL8JVHNnqJeFcvyBCm7hJQBKPBZJWH334eGe5cE1m1hKM3l8nP3kcICVLiEEuXLfycQKpBnnhRtWmuWsLBkZtEucNYa8BkCJMiTFrJ/RLgHJjWc+vqyqsiMthGePo5SWsP2ohKWpamdZBqQbz1AvnjD6oCsI7/RM+8whTHljf8RrzWLlTLoXUB60LqMf6NP34T+T+RH/HOKLJ+ho1iAAAAAElFTkSuQmCC"
                            }
            },
         ]

def create_incomplete_shared_wallet(m, acc_ix, acc_xpub)
  script_template = { 'cosigners' =>
                        { 'cosigner#0' => acc_xpub },
                      'template' =>
                          { 'all' =>
                             [ 'cosigner#0',
                               'cosigner#1'
                             ]
                          }
                      }
  pscript = script_template
  dscript = script_template
  if (m.kind_of? Array)
    payload = { mnemonic_sentence: m,
                passphrase: PASS,
                name: "Shared wallet",
                account_index: acc_ix,
                payment_script_template: pscript,
                delegation_script_template: dscript,
                }
  else
    payload = { account_public_key: m,
                passphrase: PASS,
                name: "Shared wallet",
                account_index: acc_ix,
                payment_script_template: pscript,
                delegation_script_template: dscript
                }
  end

  SHARED.wallets.create(payload)['id']
end

def create_active_shared_wallet(m, acc_ix, acc_xpub)
  script_template = { 'cosigners' =>
                        { 'cosigner#0' => acc_xpub },
                      'template' =>
                          { 'all' =>
                             [ 'cosigner#0'
                             ]
                          }
                      }
  pscript = script_template
  dscript = script_template
  if (m.kind_of? Array)
    payload = { mnemonic_sentence: m,
                passphrase: PASS,
                name: "Shared wallet",
                account_index: acc_ix,
                payment_script_template: pscript,
                delegation_script_template: dscript,
                }
  else
    payload = { account_public_key: m,
                passphrase: PASS,
                name: "Shared wallet",
                account_index: acc_ix,
                payment_script_template: pscript,
                delegation_script_template: dscript
                }
  end

  SHARED.wallets.create(payload)['id']
end

def wait_for_shared_wallet_to_sync(wid)
  puts "Syncing Shared wallet..."
  retry_count = 10
  begin
    while (SHARED.wallets.get(wid)['state']['status'] == "syncing") do
      w = SHARED.wallets.get(wid)
      puts "  Syncing... #{w['state']['progress']['quantity']}%" if w['state']['progress']
      sleep 5
    end
  rescue NoMethodError
    puts "Retry #{retry_count}"
    retry_count -= 1
    puts "SHARED.wallets.get(#{wid}) returned:"
    puts SHARED.wallets.get(wid)
    retry if retry_count > 0
  end
end

def wait_for_all_shared_wallets(wids)
  wids.each do |w|
    wait_for_shared_wallet_to_sync(w)
  end
end

def create_shelley_wallet(name = "Wallet from mnemonic_sentence", mnemonic_sentence = mnemonic_sentence(24))
  SHELLEY.wallets.create({ name: name,
                          passphrase: PASS,
                          mnemonic_sentence: mnemonic_sentence
                         })['id']
end


def create_fixture_shelley_wallet
  SHELLEY.wallets.create({ name: "Fixture wallet with funds",
                          passphrase: PASS,
                          mnemonic_sentence: get_fixture_wallet_mnemonics("shelley")
                         })['id']
end

def wait_for_shelley_wallet_to_sync(wid)
  puts "Syncing Shelley wallet..."
  retry_count = 10
  begin
    while (SHELLEY.wallets.get(wid)['state']['status'] == "syncing") do
      w = SHELLEY.wallets.get(wid)
      puts "  Syncing... #{w['state']['progress']['quantity']}%" if w['state']['progress']
      sleep 5
    end
  rescue NoMethodError
    puts "Retry #{retry_count}"
    retry_count -= 1
    puts "SHELLEY.wallets.get(#{wid}) returned:"
    puts SHELLEY.wallets.get(wid)
    retry if retry_count > 0
  end
end

def wait_for_all_shelley_wallets(wids)
  wids.each do |w|
    wait_for_shelley_wallet_to_sync(w)
  end
end

def create_byron_wallet_with(mnem, style = "random", name = "Wallet from mnemonic_sentence")
  BYRON.wallets.create({ style: style,
                        name: name,
                        passphrase: PASS,
                        mnemonic_sentence: mnem
                       })['id']
end

def create_byron_wallet(style = "random", name = "Wallet from mnemonic_sentence")
  style == "random" ? mnem = mnemonic_sentence(12) : mnem = mnemonic_sentence(15)
  BYRON.wallets.create({ style: style,
                        name: name,
                        passphrase: PASS,
                        mnemonic_sentence: mnem
                       })['id']
end


def create_fixture_byron_wallet(style = "random")
  BYRON.wallets.create({ style: style,
                        name: "Fixture byron wallets with funds",
                        passphrase: PASS,
                        mnemonic_sentence: get_fixture_wallet_mnemonics(style)
                       })['id']
end

def wait_for_byron_wallet_to_sync(wid)
  puts "Syncing Byron wallet..."
  retry_count = 10
  begin
    while (BYRON.wallets.get(wid)['state']['status'] == "syncing") do
      w = BYRON.wallets.get(wid)
      puts "  Syncing... #{w['state']['progress']['quantity']}%" if w['state']['progress']
      sleep 5
    end
  rescue NoMethodError
    puts "Retry #{retry_count}"
    retry_count -= 1
    puts "BYRON.wallets.get(#{wid}) returned:"
    puts BYRON.wallets.get(wid)
    retry if retry_count > 0
  end
end

def wait_for_all_byron_wallets(wids)
  wids.each do |w|
    wait_for_byron_wallet_to_sync(w)
  end
end

##
# wait until action passed as &block returns true or TIMEOUT is reached
def eventually(label, &block)
  current_time = Time.now
  timeout_treshold = current_time + TIMEOUT
  while (block.call == false) && (current_time <= timeout_treshold) do
    sleep 5
    current_time = Time.now
  end
  if (current_time > timeout_treshold)
    fail "Action '#{label}' did not resolve within timeout: #{TIMEOUT}s"
  end
end

def teardown
  wb = BYRON.wallets
  wb.list.each do |w|
    wb.delete w['id']
  end

  ws = SHELLEY.wallets
  ws.list.each do |w|
    ws.delete w['id']
  end

  wsh = SHARED.wallets
  wsh.list.each do |w|
    wsh.delete w['id']
  end
end

##
# return asset total or available balance for comparison
def assets_balance(assets, received = 0)
  assets.map { |x| { "#{x['policy_id']}#{x['asset_name']}" => x['quantity'] + received } }.to_set
end

##
# return ada and asset accounts balance for wallet
# identified by wallet_api
def get_wallet_balances(wid, wallet_api)
  w = wallet_api.wallets.get(wid)
  total = w['balance']['total']['quantity']
  available = w['balance']['available']['quantity']
  reward = w['balance']['reward']['quantity']
  assets_total = w['assets']['total']
  assets_available = w['assets']['available']
  { 'total' => total,
   'available' => available,
   'rewards' => reward,
   'assets_available' => assets_available,
   'assets_total' => assets_total
  }
end

def get_shelley_balances(wid)
  get_wallet_balances(wid, SHELLEY)
end

def get_shared_balances(wid)
  get_wallet_balances(wid, SHARED)
end

def get_byron_balances(wid)
  w = BYRON.wallets.get(wid)
  total = w['balance']['total']['quantity']
  available = w['balance']['available']['quantity']
  assets_total = w['assets']['total']
  assets_available = w['assets']['available']
  { 'total' => total,
   'available' => available,
   'assets_available' => assets_available,
   'assets_total' => assets_total
  }
end

##
# verify ADA balance on src and target wallets after transaction for amt ADA
# incurring fee ADA
def verify_ada_balance(src_after, src_before, target_after, target_before, amt, fee)
  expect(target_after['available']).to eq (amt + target_before['available'])
  expect(target_after['total']).to eq (amt + target_before['total'])

  expect(src_after['available']).to eq (src_before['available'] - amt - fee)
  expect(src_after['total']).to eq (src_before['total'] - amt - fee)
end

def verify_asset_balance(src_after, src_before, target_after, target_before, amt)
  if target_before['assets_total'] == []
    target_balance = [{ "#{ASSETS[0]["policy_id"]}#{ASSETS[0]["asset_name"]}" => amt },
                      { "#{ASSETS[1]["policy_id"]}#{ASSETS[1]["asset_name"]}" => amt }].to_set
    expect(assets_balance(target_after['assets_total'])).to eq target_balance
    expect(assets_balance(target_after['assets_available'])).to eq target_balance
  else
    expect(assets_balance(target_after['assets_total'])).to eq assets_balance(target_before['assets_total'], (+amt))
    expect(assets_balance(target_after['assets_available'])).to eq assets_balance(target_before['assets_available'], (+amt))
  end

  expect(assets_balance(src_after['assets_total'])).to eq assets_balance(src_before['assets_total'], (-amt))
  expect(assets_balance(src_after['assets_available'])).to eq assets_balance(src_before['assets_available'], (-amt))
end

def wait_for_tx_in_ledger(wid, tx_id)
  eventually "Tx #{tx_id} is in ledger" do
    tx = SHELLEY.transactions.get(wid, tx_id)
    tx.code == 200 && tx['status'] == 'in_ledger'
  end
end

## Plutus helpers
PLUTUS_DIR = "fixtures/plutus"

##
# Encode input index the way Plutus does in hex-encoded CBOR script
# _probably_ should work fine for 0-127 index range
# @param [Int] input idx
# @return [Hex] hex encoded input idx imitating Plutus bit-wise, non-standard encoding
#
# @example
#      > plutus_encode_idx(43)
#      => "158c"
#
# @see lib/core-integration/aux/Plutus/FlatInteger.hs
#      reference Haskell impl of encoding Int into sequence of bits
# @see lib/core-integration/src/Test/Integration/Plutus.hs
#      the way it's done in the integration tests
#      ```
#      idxEncoded = toHex $ BS.pack $ Bits.asBytes
#                  $ toBits "00" <> Bits.bits (fromIntegral idx :: Integer) <> toBits "001100"
#      ```
def plutus_encode_idx(int)
  raise "Not supported index. (0-127) are supported." if int > 127
  # convert int to binary and add trailing bit
  b = int.to_s(2) + "0"
  # add additional leading bits so it is 8-bit long
  b = "0" * (8 - b.length) + b
  # add additional leading and trailing bits
  b = "00" + b + "001100"
  # convert to hex and add leading 0's if needed (so it is 4 digit long)
  h = binary_to_hex(b)
  h = "0" * (4 - h.length) + h
  h
end

##
# Balance -> Sign -> Submit
def balance_sign_submit(wid, payload)
  tx_balanced = SHELLEY.transactions.balance(wid, payload)
  expect(tx_balanced).to be_correct_and_respond 202

  tx_signed = SHELLEY.transactions.sign(wid, PASS, tx_balanced['transaction'])
  expect(tx_signed).to be_correct_and_respond 202

  tx_submitted = PROXY.submit_external_transaction(Base64.decode64(tx_signed['transaction']))
  expect(tx_submitted).to be_correct_and_respond 202

  [tx_balanced, tx_signed, tx_submitted]
end

def get_plutus_tx(file)
  JSON.parse(File.read(File.join(PLUTUS_DIR, file)))
end

def read_mustached_file(file, ctx = {})
  Mustache.render(File.read(File.join(PLUTUS_DIR, file)), ctx).strip
end

def get_templated_plutus_tx(file, ctx = {})
  JSON.parse(read_mustached_file(file, ctx))
end

##
# Get policyId of base16-encoded minting policy
# which is Blake2b (28 byte long) hash of (script tag = 0x01 + policy)
def get_policy_id(policy)
  key = Blake2b::Key.none
  policy_id = Blake2b.hex(hex_to_bytes("01#{policy}"), key, 28)
  policy_id
end

##
# Get all sent ADA amounts from the wallet from decoded tx outputs
# We assume multi output transaction
def get_sent_amts(outputs)
  outputs.map{|o| o["amount"]["quantity"] if o["derivation_path"] == nil }
end
##
# The same as get_sent_amts, but we assume single output tx
def get_sent_amt(outputs)
  get_sent_amts(outputs).first
end
