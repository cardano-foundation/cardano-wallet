require "bundler/setup"
require "cardano_wallet"
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
TIMEOUT = 120

##
# Intit cardano-wallet wrapper with timeout for getting the response back
CW = CardanoWallet.new({ timeout: TIMEOUT })
BYRON = CW.byron
SHELLEY = CW.shelley
SHARED = CW.shared
SETTINGS = CW.misc.settings
UTILS = CW.misc.utils
NETWORK = CW.misc.network

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
ASSETS = [ { "policy_id" => "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1",
             "asset_name" => "",
             "fingerprint" => "asset1656gm7zkherdvxkn52mhaxkkw343qtkqgv0h8c",
             "metadata" => { "name" => "SadCoin",
                            "description" => "Coin with no asset name",
                            "url" => "https://sad.io",
                            "ticker" => "SAD",
                            "logo" => "iVBORw0KGgoAAAANSUhEUgAAAOIAAADbCAMAAABHu72AAAADAFBMVEUAAAAAADMAAGYAAJkAAMwAAP8AMwAAMzMAM2YAM5kAM8wAM/8AZgAAZjMAZmYAZpkAZswAZv8AmQAAmTMAmWYAmZkAmcwAmf8AzAAAzDMAzGYAzJkAzMwAzP8A/wAA/zMA/2YA/5kA/8wA//8zAAAzADMzAGYzAJkzAMwzAP8zMwAzMzMzM2YzM5kzM8wzM/8zZgAzZjMzZmYzZpkzZswzZv8zmQAzmTMzmWYzmZkzmcwzmf8zzAAzzDMzzGYzzJkzzMwzzP8z/wAz/zMz/2Yz/5kz/8wz//9mAABmADNmAGZmAJlmAMxmAP9mMwBmMzNmM2ZmM5lmM8xmM/9mZgBmZjNmZmZmZplmZsxmZv9mmQBmmTNmmWZmmZlmmcxmmf9mzABmzDNmzGZmzJlmzMxmzP9m/wBm/zNm/2Zm/5lm/8xm//+ZAACZADOZAGaZAJmZAMyZAP+ZMwCZMzOZM2aZM5mZM8yZM/+ZZgCZZjOZZmaZZpmZZsyZZv+ZmQCZmTOZmWaZmZmZmcyZmf+ZzACZzDOZzGaZzJmZzMyZzP+Z/wCZ/zOZ/2aZ/5mZ/8yZ///MAADMADPMAGbMAJnMAMzMAP/MMwDMMzPMM2bMM5nMM8zMM//MZgDMZjPMZmbMZpnMZszMZv/MmQDMmTPMmWbMmZnMmczMmf/MzADMzDPMzGbMzJnMzMzMzP/M/wDM/zPM/2bM/5nM/8zM////AAD/ADP/AGb/AJn/AMz/AP//MwD/MzP/M2b/M5n/M8z/M///ZgD/ZjP/Zmb/Zpn/Zsz/Zv//mQD/mTP/mWb/mZn/mcz/mf//zAD/zDP/zGb/zJn/zMz/zP///wD//zP//2b//5n//8z///8AAACh0KGj0aNHo0dut27B4ME+nz4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABpSVAoAAABAHRSTlP///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////8A////////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAhTASYwAAAAlwSFlzAAAOxAAADsQBlSsOGwAABS9JREFUeJztnU2eoyAQxfsMcxkv48rLeBlWXsbrjJhfz6RjkHpVD0yX9Va9SAf+1BcYwK/Vvb6u7kB7BaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBSJHS5qnYdOfrPzHNKelR8O7WiMu8/ggO2oY5y6cTRG/8XazpeUBtCxpN+o3ZssO7GqHOA8VU30beGhM2QgxjbnzdU/cMPcPpjbd2NUEcTeg2AV3yoambICYAcEeK/5FLjrinE2C+13Khm8DSUbMPVUGVg5fxdjURUVcci/1tW7J48MvlUzE2RxQOSbp3kpE3Lo3mb9k2obJ3pUfoiEmk4/+V/ZWbkSyECeGCelftYuESI2hmeusFMTNuShO2ub7GIi5R4SveRaTkYC4JZrR/i0vGnlJx46Y2ky8JhqjGXFpNbXckg7HV62IzQh5jEbELdPIa1ia8pppEvvfxMk5RsRBnmnSv8dU4vXESMnUNkSgD/scfcmPMoBpAjCCZZkQZznh9MQ1y2doAyHSLYhJntd/loAkZkyElGNBlA/x/DIY8loKOEpJBsRRHCjHyiIvCPJWStIjbpaQfnQ82mwW99w8y9Ejyt00vXO2Qdpzs6uqEYGW39K85S78uy2rahEXeaorDIa450BTb6VFfBNeJRV6uIhjWR63b6VElPev3EH5KNnMqEQEjFjMK/JotJlRhwgYcSmDDGLjmMyoQwSMOJfnahMwOzKYUYUIGPHMVCcGfpXFjCrESb4OPsWQeyrQ4kEqRHnXzvwU8VTA4AdpEOWpMEftyTwtyUNMPN87SoMIJJv1PGoJ1bUuDSKQbCoOBng80OiLFIjIgJ6GIhKMkOv8lAIR6Nd5KELBWBmsEykQAe+qfRZIlPqciiNCbdUiCIgwdfXHEZFQrA4H4BHqYMQRkaaqFRQod+qygSMioVjNEUDqUgcjjogUqCoBkie1lRFGhAaz6tSI9yHu8ywYEQqJaqhh011dvlEgAiWYiohMOZ4FI0JjWfUtxO21KRVGhFY1VETE4s9qi1hPgkCa7IgI5DUqorYwBuJRUAGmImprfyAa26Fm1EAsqm26oc5uPjOj/lZEoPRTVxqfObuhrhe7IULTcOaqv980HFpMMZ/d9FtMQWPJfALXb0mM5TXic9R+Dzaotb9H5W/8kJH4m0bPh4zMlNojoTZ+4E/8fbHnA39mvsGWUt1+tiEGY5dQbP0TKmvHRuefUHnB2CUUW29nYO2e6rydgVY2upSM9luLODsZu28tIq38e6z4s1pv86PsKr5gmx+yWZOwN7z8HSI133Jr3+G/XrHlFjSj8ZzGesnGacyMttM26zXb30Ezms5MrRcdYsDMaDn5tl51FAU85aM/vwg3dVSPY2Gr/hRq1lXHwtCWlWeJ1ysP9yFHNLNUJ8KzLjyiiR6BVZzrV7TyRn2OSz+E3s6Qde1xaeTQu6GJSw+9M4a4oquvLmBdglHW9RdQkPpQ1CdcI4JdBoPqMy6DucGVPne4mAmdcIr1Qddr3eGStDtcdXeHCwvXG1w7ud7h8tA7XAG73uAi3/UO1zHf4VLt9QZXo2e5v+A+y/1rCrLAl00Mv+5lE1nuXxmyy/uLXx7y/vqeh7y/hOlbvl+l9QEKRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA+6AeJfIVsvZgFCRMcAAAAASUVORK5CYII="
                            }
           },
           { "policy_id" => "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1",
             "asset_name" => "6861707079636f696e",
             "fingerprint" => "asset19mwamgpre24at3z34v2e5achszlhhqght9djqp",
             "metadata" => { "name" => "HappyCoin",
                            "description" => "Coin with asset name - and everyone is happy!!!",
                            "url" => "https://happy.io",
                            "ticker" => "HAPP",
                            "logo" => "iVBORw0KGgoAAAANSUhEUgAAAOIAAADbCAMAAABHu72AAAADAFBMVEUAAAAAADMAAGYAAJkAAMwAAP8AMwAAMzMAM2YAM5kAM8wAM/8AZgAAZjMAZmYAZpkAZswAZv8AmQAAmTMAmWYAmZkAmcwAmf8AzAAAzDMAzGYAzJkAzMwAzP8A/wAA/zMA/2YA/5kA/8wA//8zAAAzADMzAGYzAJkzAMwzAP8zMwAzMzMzM2YzM5kzM8wzM/8zZgAzZjMzZmYzZpkzZswzZv8zmQAzmTMzmWYzmZkzmcwzmf8zzAAzzDMzzGYzzJkzzMwzzP8z/wAz/zMz/2Yz/5kz/8wz//9mAABmADNmAGZmAJlmAMxmAP9mMwBmMzNmM2ZmM5lmM8xmM/9mZgBmZjNmZmZmZplmZsxmZv9mmQBmmTNmmWZmmZlmmcxmmf9mzABmzDNmzGZmzJlmzMxmzP9m/wBm/zNm/2Zm/5lm/8xm//+ZAACZADOZAGaZAJmZAMyZAP+ZMwCZMzOZM2aZM5mZM8yZM/+ZZgCZZjOZZmaZZpmZZsyZZv+ZmQCZmTOZmWaZmZmZmcyZmf+ZzACZzDOZzGaZzJmZzMyZzP+Z/wCZ/zOZ/2aZ/5mZ/8yZ///MAADMADPMAGbMAJnMAMzMAP/MMwDMMzPMM2bMM5nMM8zMM//MZgDMZjPMZmbMZpnMZszMZv/MmQDMmTPMmWbMmZnMmczMmf/MzADMzDPMzGbMzJnMzMzMzP/M/wDM/zPM/2bM/5nM/8zM////AAD/ADP/AGb/AJn/AMz/AP//MwD/MzP/M2b/M5n/M8z/M///ZgD/ZjP/Zmb/Zpn/Zsz/Zv//mQD/mTP/mWb/mZn/mcz/mf//zAD/zDP/zGb/zJn/zMz/zP///wD//zP//2b//5n//8z///8AAACh0KGj0aNHo0dut27B4ME+nz4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABpSVAoAAABAHRSTlP///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////8A////////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAhTASYwAAAAlwSFlzAAAOxAAADsQBlSsOGwAABS9JREFUeJztnU2eoyAQxfsMcxkv48rLeBlWXsbrjJhfz6RjkHpVD0yX9Va9SAf+1BcYwK/Vvb6u7kB7BaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBaIHBSJHS5qnYdOfrPzHNKelR8O7WiMu8/ggO2oY5y6cTRG/8XazpeUBtCxpN+o3ZssO7GqHOA8VU30beGhM2QgxjbnzdU/cMPcPpjbd2NUEcTeg2AV3yoambICYAcEeK/5FLjrinE2C+13Khm8DSUbMPVUGVg5fxdjURUVcci/1tW7J48MvlUzE2RxQOSbp3kpE3Lo3mb9k2obJ3pUfoiEmk4/+V/ZWbkSyECeGCelftYuESI2hmeusFMTNuShO2ub7GIi5R4SveRaTkYC4JZrR/i0vGnlJx46Y2ky8JhqjGXFpNbXckg7HV62IzQh5jEbELdPIa1ia8pppEvvfxMk5RsRBnmnSv8dU4vXESMnUNkSgD/scfcmPMoBpAjCCZZkQZznh9MQ1y2doAyHSLYhJntd/loAkZkyElGNBlA/x/DIY8loKOEpJBsRRHCjHyiIvCPJWStIjbpaQfnQ82mwW99w8y9Ejyt00vXO2Qdpzs6uqEYGW39K85S78uy2rahEXeaorDIa450BTb6VFfBNeJRV6uIhjWR63b6VElPev3EH5KNnMqEQEjFjMK/JotJlRhwgYcSmDDGLjmMyoQwSMOJfnahMwOzKYUYUIGPHMVCcGfpXFjCrESb4OPsWQeyrQ4kEqRHnXzvwU8VTA4AdpEOWpMEftyTwtyUNMPN87SoMIJJv1PGoJ1bUuDSKQbCoOBng80OiLFIjIgJ6GIhKMkOv8lAIR6Nd5KELBWBmsEykQAe+qfRZIlPqciiNCbdUiCIgwdfXHEZFQrA4H4BHqYMQRkaaqFRQod+qygSMioVjNEUDqUgcjjogUqCoBkie1lRFGhAaz6tSI9yHu8ywYEQqJaqhh011dvlEgAiWYiohMOZ4FI0JjWfUtxO21KRVGhFY1VETE4s9qi1hPgkCa7IgI5DUqorYwBuJRUAGmImprfyAa26Fm1EAsqm26oc5uPjOj/lZEoPRTVxqfObuhrhe7IULTcOaqv980HFpMMZ/d9FtMQWPJfALXb0mM5TXic9R+Dzaotb9H5W/8kJH4m0bPh4zMlNojoTZ+4E/8fbHnA39mvsGWUt1+tiEGY5dQbP0TKmvHRuefUHnB2CUUW29nYO2e6rydgVY2upSM9luLODsZu28tIq38e6z4s1pv86PsKr5gmx+yWZOwN7z8HSI133Jr3+G/XrHlFjSj8ZzGesnGacyMttM26zXb30Ezms5MrRcdYsDMaDn5tl51FAU85aM/vwg3dVSPY2Gr/hRq1lXHwtCWlWeJ1ysP9yFHNLNUJ8KzLjyiiR6BVZzrV7TyRn2OSz+E3s6Qde1xaeTQu6GJSw+9M4a4oquvLmBdglHW9RdQkPpQ1CdcI4JdBoPqMy6DucGVPne4mAmdcIr1Qddr3eGStDtcdXeHCwvXG1w7ud7h8tA7XAG73uAi3/UO1zHf4VLt9QZXo2e5v+A+y/1rCrLAl00Mv+5lE1nuXxmyy/uLXx7y/vqeh7y/hOlbvl+l9QEKRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA8KRA+6AeJfIVsvZgFCRMcAAAAASUVORK5CYII="
                            }
            },
         ]

def create_pending_shared_wallet(m, acc_ix, acc_xpub)
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

def create_shelley_wallet(name = "Wallet from mnemonic_sentence")
  SHELLEY.wallets.create({ name: name,
                          passphrase: PASS,
                          mnemonic_sentence: mnemonic_sentence(24)
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
end
