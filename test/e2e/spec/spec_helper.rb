require "bip_mnemonic"
require "bundler/setup"
require "cardano_wallet"


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

PASS = "Secure Passphrase"
# Artificial, non-existing id's
TXID = "1acf9c0f504746cbd102b49ffaf16dcafd14c0a2f1bbb23af265fbe0a04951cc"
SPID = "feea59bc6664572e631e9adfee77142cb51264156debf2e52970cc00"
SPID_BECH32 = "pool1v7g9ays8h668d74xjvln9xuh9adzh6xz0v0hvcd3xukpck5z56d"
###
BYRON = CardanoWallet.new.byron
SHELLEY = CardanoWallet.new.shelley
# timeout in seconds for custom verifications
TIMEOUT = 120

#exemplary metadata
METADATA = { "0"=>{ "string"=>"cardano" },
             "1"=>{ "int"=>14 },
             "2"=>{ "bytes"=>"2512a00e9653fe49a44a5886202e24d77eeb998f" },
             "3"=>{ "list"=>[ { "int"=>14 }, { "int"=>42 }, { "string"=>"1337" } ] },
             "4"=>{ "map"=>[ { "k"=>{ "string"=>"key" }, "v"=>{ "string"=>"value" } },
                             { "k"=>{ "int"=>14 }, "v"=>{ "int"=>42 } } ] } }

# Testnet assets with metadata from mock server http://metadata-server-mock.herokuapp.com/
ASSETS = [ { "policy_id" => "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1",
             "asset_name" => "",
             "fingerprint" => "asset1656gm7zkherdvxkn52mhaxkkw343qtkqgv0h8c",
             "metadata" => {"name" => "SadCoin",
                            "description" => "Coin with no asset name",
                            "url" => "https://sad.io",
                            "ticker" => "SAD",
                            "logo" => "QWxtb3N0IGEgbG9nbw==",
                            "unit" => {"name" => "saddies", "decimals" => 10}
                            }
           },
           { "policy_id" => "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1",
             "asset_name" => "6861707079636f696e",
             "fingerprint" => "asset19mwamgpre24at3z34v2e5achszlhhqght9djqp",
             "metadata" => {"name" => "HappyCoin",
                            "description" => "Coin with asset name",
                            "url" => "https://happy.io",
                            "ticker" => "HAPP",
                            "logo" => "QWxtb3N0IGEgbG9nbw==",
                            "unit" => {"name" => "happies", "decimals" => 19}
                            }
            },
         ]

def create_shelley_wallet(name = "Wallet from mnemonic_sentence")
  SHELLEY.wallets.create({name: name,
                          passphrase: PASS,
                          mnemonic_sentence: mnemonic_sentence("24")
                         })['id']
end


def create_fixture_shelley_wallet
  # Wallet with funds on shelley testnet:
  # id: b1fb863243a9ae451bc4e2e662f60ff217b126e2
  # addr: addr_test1qq9grthf479qmyygzrenk6yqqhtvf3aq2xy5jfscm334qsvs47mevx68ut5g3jt5gxntcaygv3szmhzyytdjfat9758s2h6z2v
  mnemonics = %w[shiver unknown lottery calm renew west any ecology merge slab sort color hybrid pact crowd]
  SHELLEY.wallets.create({name: "Fixture wallet with funds",
                          passphrase: PASS,
                          mnemonic_sentence: mnemonics
                         })['id']
end

def wait_for_shelley_wallet_to_sync(wid)
  puts "Syncing Shelley wallet..."
  while(SHELLEY.wallets.get(wid)['state']['status'] == "syncing") do
    w = SHELLEY.wallets.get(wid)
    puts "  Syncing... #{w['state']['progress']['quantity']}%" if w['state']['progress']
    sleep 5
  end
end

def wait_for_all_shelley_wallets(wids)
  wids.each do |w|
    wait_for_shelley_wallet_to_sync(w)
  end
end

def create_byron_wallet_with(mnem, style = "random", name = "Wallet from mnemonic_sentence")
  BYRON.wallets.create({style: style,
                        name: name,
                        passphrase: PASS,
                        mnemonic_sentence: mnem
                       })['id']
end

def create_byron_wallet(style = "random", name = "Wallet from mnemonic_sentence")
  style == "random" ? mnem = mnemonic_sentence("12") : mnem = mnemonic_sentence("15")
  BYRON.wallets.create({style: style,
                        name: name,
                        passphrase: PASS,
                        mnemonic_sentence: mnem
                       })['id']
end


def create_fixture_byron_wallet(style = "random")
  # Wallet with funds on shelley testnet
  case style
  when "random"
    # id: 94c0af1034914f4455b7eb795ebea74392deafe9
    # addr: 37btjrVyb4KEciULDrqJDBh6SjgPqi9JJ5qQqWGgvtsB7GcsuqorKceMTBRudFK8zDu3btoC5FtN7K1PEHmko4neQPfV9TDVfivc9JTZVNPKtRd4w2
    mnemonics = %w[purchase carbon forest frog robot actual used news broken start plunge family]
  when "icarus"
    # id: a468e96ab85ad2043e48cf2e5f3437b4356769f4
    # addr: 2cWKMJemoBahV5kQm7SzV7Yc2b4vyqLE7oYJiEkd5GE5GCKzSCgh9HBaRKkdVrxzsEuRb
    mnemonics = %w[security defense food inhale voyage tomorrow guess galaxy junior guilt vendor soon escape design pretty]
  end

  BYRON.wallets.create({style: style,
                        name: "Fixture byron wallets with funds",
                        passphrase: PASS,
                        mnemonic_sentence: mnemonics
                       })['id']
end

def wait_for_byron_wallet_to_sync(wid)
  puts "Syncing Byron wallet..."
  while BYRON.wallets.get(wid)['state']['status'] == "syncing" do
    w = BYRON.wallets.get(wid)
    puts "  Syncing... #{w['state']['progress']['quantity']}%" if w['state']['progress']
    sleep 5
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
  while(block.call == false) && (current_time <= timeout_treshold) do
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
end

def mnemonic_sentence(word_count = "15")
  case word_count
    when '9'
      bits = 96
    when '12'
      bits = 128
    when '15'
      bits = 164
    when '18'
      bits = 196
    when '21'
      bits = 224
    when '24'
      bits = 256
    else
      raise "Non-supported no of words #{word_count}!"
  end
  BipMnemonic.to_mnemonic(bits: bits, language: 'english').split
end
