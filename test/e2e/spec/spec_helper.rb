# frozen_string_literal: true

require 'bundler/setup'
require 'cardano_wallet'
require 'base64'
require 'blake2b'
require 'mustache'
require 'cbor'
require 'tmpdir'
require 'set'
require_relative '../env'
require_relative '../helpers/utils'
require_relative '../helpers/tx_history'
require_relative '../helpers/matchers'
require_relative '../helpers/context'
require_relative '../helpers/wallet_factory'
require_relative '../helpers/cardano_addresses'
require_relative '../helpers/cardano_cli'

include Helpers::Utils
include TxHistory

RSpec.configure do |config|
  # Enable flags like --only-failures and --next-failure
  config.example_status_persistence_file_path = '.rspec_status'

  # Disable RSpec exposing methods globally on `Module` and `main`
  config.disable_monkey_patching!

  config.expect_with :rspec do |c|
    c.syntax = :expect
    c.max_formatted_output_length = nil
  end
end

# Helpers

##
# timeout in seconds for custom verifications
TIMEOUT = 600
FIXTURES_DIR = 'fixtures'

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
NODE = CW.misc.node
CA = CardanoAddresses.new

CONTEXT = Context.new
CONTEXT.env = ENV.fetch('NETWORK', nil)

CARDANO_CLI = CardanoCli.new(get_protocol_magic(CONTEXT.env))

##
# default passphrase for wallets
PASS = 'Secure Passphrase'

##
# Artificial, non-existing id's
TXID = '1acf9c0f504746cbd102b49ffaf16dcafd14c0a2f1bbb23af265fbe0a04951cc'
SPID = 'feea59bc6664572e631e9adfee77142cb51264156debf2e52970cc00'
SPID_BECH32 = 'pool1v7g9ays8h668d74xjvln9xuh9adzh6xz0v0hvcd3xukpck5z56d'
DEV_NULL_ADDR = 'addr_test1qp760qtlwv6cyvvkpz3a6s0y72aea4xd4da85rm5qe2u6awgscyn6cz7plwgtanjanvpg9xt4lc3wlrqhcw5cmxk334q0wca8l'

# exemplary metadata
METADATA = { '0' => { 'string' => 'cardano' },
             '1' => { 'int' => 14 },
             '2' => { 'bytes' => '2512a00e9653fe49a44a5886202e24d77eeb998f' },
             '3' => { 'list' => [{ 'int' => 14 }, { 'int' => 42 }, { 'string' => '1337' }] },
             '4' => { 'map' => [{ 'k' => { 'string' => 'key' }, 'v' => { 'string' => 'value' } },
                                { 'k' => { 'int' => 14 }, 'v' => { 'int' => 42 } }] } }.freeze

# Testnet assets with metadata from mock server https://metadata.world.dev.cardano.org/
ASSETS = [{ 'policy_id' => 'ee1ce9d7560f48a4ba3867037dbec2d8fed776d94dd6b00a35309073',
            'asset_name' => '',
            'fingerprint' => 'asset1s3yhz885gnyu2wcpz5h275u37hw3axz3c9sfqu',
            'metadata' => { 'name' => 'SadCoin',
                            'description' => 'Coin with no asset name',
                            'url' => 'https://sad.io',
                            'ticker' => 'SAD',
                            'logo' => 'iVBORw0KGgoAAAANSUhEUgAAABkAAAAeCAYAAADZ7LXbAAAACXBIWXMAAA7EAAAOxAGVKw4bAAACbUlEQVRIie3Vy0tUURzA8e855965c8lXUhlhEQVBSEmQRAURQbSIEqFl4N6/oHYtAhdtonatK8hVBCERZC+0jbZpIRVkIeagTJrO3Nd5tBhDMHOcGiHCA2dxHvDh9zs/fkc45xwbPORGA5tI/RFdGCL9MgAm/mNEVKuuaHA3OW+RlDb8zjt4O07VjFRPV8NBZC5PGMxj3/YQv7uGs7p+iJ5+ipgfIZr7hnWSXBjgT98iHr6IS+fqg7h0Dl8ZQpmQFKdJSmWkkuSj10TD3WCzv0f89m6S8BjWQehbVDpPWiojsASlEeLxG3WIJFtANneQei3EqpnMeWRxgtMahYGP/dhoqiry2+rKJh9i3l8l2KIRUlVQazDlRXTpOzIr43uQ7LlCvrO/9kjisT7Ehz6CBgtCki4sEC+ALpdQQUC+qQmXC3EO3NQAsHaP/QVx1mBnh5BKYpOYON2L6npJ/sw4svMRacmCc+TyOQwKGX/CRl9rQ4SQyPZeFqM27L7bhCcHUY37AVCtR7EtZ8EZhLN4vkIKhy1N1Ibo4ijq83UavAl04QmIFVekB1aDNQhnQFBZ14KABauRaFThHrrwbPmkPImYeQw6A5OBNRjnIxsPrIl4KzdUcwep9SFL8JVHNnqJeFcvyBCm7hJQBKPBZJWH334eGe5cE1m1hKM3l8nP3kcICVLiEEuXLfycQKpBnnhRtWmuWsLBkZtEucNYa8BkCJMiTFrJ/RLgHJjWc+vqyqsiMthGePo5SWsP2ohKWpamdZBqQbz1AvnjD6oCsI7/RM+8whTHljf8RrzWLlTLoXUB60LqMf6NP34T+T+RH/HOKLJ+ho1iAAAAAElFTkSuQmCC' } },
          { 'policy_id' => '919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149',
            'asset_name' => asset_name('HappyCoin'),
            'fingerprint' => 'asset19mwamgpre24at3z34v2e5achszlhhqght9djqp',
            'metadata' => { 'name' => 'HappyCoin',
                            'description' => 'Coin with asset name - and everyone is happy!!!',
                            'url' => 'https://happy.io',
                            'decimals' => 6,
                            'ticker' => 'HAPP',
                            'logo' => 'iVBORw0KGgoAAAANSUhEUgAAABkAAAAeCAYAAADZ7LXbAAAACXBIWXMAAA7EAAAOxAGVKw4bAAACbUlEQVRIie3Vy0tUURzA8e855965c8lXUhlhEQVBSEmQRAURQbSIEqFl4N6/oHYtAhdtonatK8hVBCERZC+0jbZpIRVkIeagTJrO3Nd5tBhDMHOcGiHCA2dxHvDh9zs/fkc45xwbPORGA5tI/RFdGCL9MgAm/mNEVKuuaHA3OW+RlDb8zjt4O07VjFRPV8NBZC5PGMxj3/YQv7uGs7p+iJ5+ipgfIZr7hnWSXBjgT98iHr6IS+fqg7h0Dl8ZQpmQFKdJSmWkkuSj10TD3WCzv0f89m6S8BjWQehbVDpPWiojsASlEeLxG3WIJFtANneQei3EqpnMeWRxgtMahYGP/dhoqiry2+rKJh9i3l8l2KIRUlVQazDlRXTpOzIr43uQ7LlCvrO/9kjisT7Ehz6CBgtCki4sEC+ALpdQQUC+qQmXC3EO3NQAsHaP/QVx1mBnh5BKYpOYON2L6npJ/sw4svMRacmCc+TyOQwKGX/CRl9rQ4SQyPZeFqM27L7bhCcHUY37AVCtR7EtZ8EZhLN4vkIKhy1N1Ibo4ijq83UavAl04QmIFVekB1aDNQhnQFBZ14KABauRaFThHrrwbPmkPImYeQw6A5OBNRjnIxsPrIl4KzdUcwep9SFL8JVHNnqJeFcvyBCm7hJQBKPBZJWH334eGe5cE1m1hKM3l8nP3kcICVLiEEuXLfycQKpBnnhRtWmuWsLBkZtEucNYa8BkCJMiTFrJ/RLgHJjWc+vqyqsiMthGePo5SWsP2ohKWpamdZBqQbz1AvnjD6oCsI7/RM+8whTHljf8RrzWLlTLoXUB60LqMf6NP34T+T+RH/HOKLJ+ho1iAAAAAElFTkSuQmCC' } }].freeze

##
# Since alonzo min_utxo_value is calculated based on the particular output size
# 1 ADA, however should be enough for sending pure Ada output to shelley address
# setting it to 2 ADA temporarily because of:
# ADP-2298 - Deposit_returned is falsely reported on some incoming transactions (intermittently)
MIN_UTXO_VALUE_PURE_ADA = 2_000_000

def payment_payload(amt, addr = DEV_NULL_ADDR)
  [{ :address => addr,
     :amount => { :quantity => amt,
                  :unit => 'lovelace' } }]
end

def create_incomplete_shared_wallet(m, acc_ix, acc_xpub)
  script_template = { 'cosigners' =>
                        { 'cosigner#0' => acc_xpub },
                      'template' =>
                          { 'all' =>
                             ['cosigner#0',
                              'cosigner#1'] } }
  pscript = script_template
  dscript = script_template
  payload = if m.is_a? Array
              { mnemonic_sentence: m,
                passphrase: PASS,
                name: 'Shared wallet',
                account_index: acc_ix,
                payment_script_template: pscript,
                delegation_script_template: dscript }
            else
              { account_public_key: m,
                passphrase: PASS,
                name: 'Shared wallet',
                account_index: acc_ix,
                payment_script_template: pscript,
                delegation_script_template: dscript }
            end
  WalletFactory.create(:shared, payload)['id']
end

def shared_acc_pubkey(wallet_id)
  SHARED.keys.get_acc_public_key(wallet_id, { format: 'extended' }).parsed_response.delete_prefix('"').delete_suffix('"')
end

def patch_incomplete_shared_wallet(wid, payment_patch, deleg_patch)
  if payment_patch
    p_upd = SHARED.wallets.update_payment_script(wid,
                                                 payment_patch.keys.first,
                                                 payment_patch.values.first)
    expect(p_upd).to be_correct_and_respond 200
  end

  return unless deleg_patch

  d_upd = SHARED.wallets.update_delegation_script(wid,
                                                  deleg_patch.keys.first,
                                                  deleg_patch.values.first)
  expect(d_upd).to be_correct_and_respond 200
end

def patch_if_incomplete(wid, payment_patch, deleg_patch)
  if payment_patch
    p_upd = SHARED.wallets.update_payment_script(wid,
                                                 payment_patch.keys.first,
                                                 payment_patch.values.first)
    case p_upd.code
    when 200
      expect(p_upd).to be_correct_and_respond 200
    when 403
      expect(p_upd).to be_correct_and_respond 403
      expect(p_upd.parsed_response['code']).to eq 'shared_wallet_active'
    end
  end

  return unless deleg_patch

  d_upd = SHARED.wallets.update_delegation_script(wid,
                                                  deleg_patch.keys.first,
                                                  deleg_patch.values.first)
  case d_upd.code
  when 200
    expect(d_upd).to be_correct_and_respond 200
  when 403
    expect(d_upd).to be_correct_and_respond 403
    expect(d_upd.parsed_response['code']).to eq 'shared_wallet_active'
  end
end

def create_active_shared_wallet(m, acc_ix, acc_xpub)
  script_template = { 'cosigners' =>
                        { 'cosigner#0' => acc_xpub },
                      'template' =>
                          { 'all' =>
                             ['cosigner#0'] } }
  pscript = script_template
  dscript = script_template
  payload = if m.is_a? Array
              { mnemonic_sentence: m,
                passphrase: PASS,
                name: 'Shared wallet',
                account_index: acc_ix,
                payment_script_template: pscript,
                delegation_script_template: dscript }
            else
              { account_public_key: m,
                passphrase: PASS,
                name: 'Shared wallet',
                account_index: acc_ix,
                payment_script_template: pscript,
                delegation_script_template: dscript }
            end

  WalletFactory.create(:shared, payload)['id']
end

def wait_for_shared_wallet_to_sync(wid)
  puts 'Syncing Shared wallet...'
  retry_count = 10
  begin
    while SHARED.wallets.get(wid)['state']['status'].to_s == 'syncing'
      w = SHARED.wallets.get(wid)
      puts "  Syncing... #{w['state']['progress']['quantity'].to_i}%" if w['state']['progress']
      sleep 5
    end
  rescue StandardError
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

def create_shelley_wallet(name = 'Wallet from mnemonic_sentence',
                          mnemonic_sentence = CW.utils.mnemonic_sentence(24),
                          mnemonic_second_factor = nil)
  payload = { name: name,
              passphrase: PASS,
              mnemonic_sentence: mnemonic_sentence }
  payload[:mnemonic_second_factor] = mnemonic_second_factor if mnemonic_second_factor
  WalletFactory.create(:shelley, payload)['id']
end

def wait_for_shelley_wallet_to_sync(wid)
  puts 'Syncing Shelley wallet...'
  retry_count = 10
  begin
    while SHELLEY.wallets.get(wid)['state']['status'].to_s == 'syncing'
      w = SHELLEY.wallets.get(wid)
      puts "  Syncing... #{w['state']['progress']['quantity'].to_i}%" if w['state']['progress']
      sleep 5
    end
  rescue StandardError
    puts "Retry #{retry_count}"
    retry_count -= 1
    puts "SHELLEY.wallets.get(#{wid}) returned:"
    puts w
    retry if retry_count > 0
  end
end

def wait_for_all_shelley_wallets(wids)
  wids.each do |w|
    wait_for_shelley_wallet_to_sync(w)
  end
end

def create_byron_wallet(style = 'random',
                        name = 'Wallet from mnemonic_sentence',
                        mnemonics = CW.utils.mnemonic_sentence(24))
  payload = { style: style,
              name: name,
              passphrase: PASS,
              mnemonic_sentence: mnemonics }
  WalletFactory.create(:byron, payload)['id']
end

def wait_for_byron_wallet_to_sync(wid)
  puts 'Syncing Byron wallet...'
  retry_count = 10
  begin
    while BYRON.wallets.get(wid)['state']['status'].to_s == 'syncing'
      w = BYRON.wallets.get(wid)
      puts "  Syncing... #{w['state']['progress']['quantity'].to_i}%" if w['state']['progress']
      sleep 5
    end
  rescue StandardError
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

## FIXTURE AND TARGET WALLETS ##

##
# return wallet id from create wallet response even if it already exists
def return_wallet_id(create_wallet_response)
  if create_wallet_response.code == 409
    create_wallet_response['message'].split[10]
  else
    create_wallet_response['id']
  end
end

##
# create fixture wallet or return it's id if it exists
# @param type [Symbol] :shelley, :shared, :shared_cosigner_0, :shared_cosigner_1, :random, :icarus
# @param templates [Symbols] ':payment_cosigner{0,1}_{all,any,all0}', :delegation_cosigner{0,1}_{all,any,all0}
# rubocop:disable Metrics/CyclomaticComplexity
def create_fixture_wallet(type, *templates)
  payload = { name: "Fixture wallet with funds (#{type}#{" #{templates}" unless templates.empty?}",
              passphrase: PASS,
              mnemonic_sentence: get_fixture_wallet(:fixture, type.to_sym, :mnemonics) }
  case type.to_sym
  when :shelley
    wallet = SHELLEY.wallets.create(payload)
    return_wallet_id(wallet)
  when :random, :icarus
    payload[:style] = type
    wallet = BYRON.wallets.create(payload)
    return_wallet_id(wallet)
  when :shared, :shared2
    templates.each do |t|
      case t
      when :payment_cosigner0_all
        payload[:payment_script_template] = { 'cosigners' => { 'cosigner#0' => 'self' }, 'template' => { 'all' => ['cosigner#0', 'cosigner#1'] } }
      when :delegation_cosigner0_all
        payload[:delegation_script_template] = { 'cosigners' => { 'cosigner#0' => 'self' }, 'template' => { 'all' => ['cosigner#0', 'cosigner#1'] } }
      when :payment_cosigner1_all
        payload[:payment_script_template] = { 'cosigners' => { 'cosigner#1' => 'self' }, 'template' => { 'all' => ['cosigner#0', 'cosigner#1'] } }
      when :delegation_cosigner1_all
        payload[:delegation_script_template] = { 'cosigners' => { 'cosigner#1' => 'self' }, 'template' => { 'all' => ['cosigner#0', 'cosigner#1'] } }
      when :payment_cosigner0_any
        payload[:payment_script_template] = { 'cosigners' => { 'cosigner#0' => 'self' }, 'template' => { 'any' => ['cosigner#0', 'cosigner#1'] } }
      when :delegation_cosigner0_any
        payload[:delegation_script_template] = { 'cosigners' => { 'cosigner#0' => 'self' }, 'template' => { 'any' => ['cosigner#0', 'cosigner#1'] } }
      when :payment_cosigner1_any
        payload[:payment_script_template] = { 'cosigners' => { 'cosigner#1' => 'self' }, 'template' => { 'any' => ['cosigner#0', 'cosigner#1'] } }
      when :delegation_cosigner1_any
        payload[:delegation_script_template] = { 'cosigners' => { 'cosigner#1' => 'self' }, 'template' => { 'any' => ['cosigner#0', 'cosigner#1'] } }
      when :payment_cosigner0_all0
        payload[:payment_script_template] = { 'cosigners' => { 'cosigner#0' => 'self' }, 'template' => { 'all' => ['cosigner#0'] } }
      when :delegation_cosigner0_all0
        payload[:delegation_script_template] = { 'cosigners' => { 'cosigner#0' => 'self' }, 'template' => { 'all' => ['cosigner#0'] } }
      end
    end
    payload[:account_index] = '0H'
    wallet = SHARED.wallets.create(payload)
    return_wallet_id(wallet)
  else
    raise "Unsupported wallet type: #{type}"
  end
end
# rubocop:enable Metrics/CyclomaticComplexity

##
# create target wallet or return it's id if it exists
# @param type [Symbol] :shelley, :shared
def create_target_wallet(type)
  payload = { name: 'Target wallet for txs',
              passphrase: PASS,
              mnemonic_sentence: get_fixture_wallet(:target, type.to_sym, :mnemonics) }
  case type.to_sym
  when :shelley
    wallet = SHELLEY.wallets.create(payload)
    return_wallet_id(wallet)
  else
    raise "Unsupported wallet type: #{type}"
  end
end

##
# wait until action passed as &block returns true or TIMEOUT is reached
def eventually(label, &block)
  current_time = Time.now
  timeout_treshold = current_time + TIMEOUT
  while (block.call == false) && (current_time <= timeout_treshold)
    sleep 5
    current_time = Time.now
  end
  raise "Action '#{label}' did not resolve within timeout: #{TIMEOUT}s" if current_time > timeout_treshold
end

def teardown
  CONTEXT.byron.dup.each do |wid|
    WalletFactory.delete(:byron, wid)
  end

  CONTEXT.shelley.dup.each do |wid|
    WalletFactory.delete(:shelley, wid)
  end

  CONTEXT.shared.dup.each do |wid|
    WalletFactory.delete(:shared, wid)
  end
end

##
# return asset total or available balance for comparison
# @param [Hash] assets - asset balance Hash from the wallet (output of get_wallet_balances['assets_*'])
# @param [Hash] options -
#                    options[:delta] [Int] - received/sent assets that we are expecting (default: 0)
#                    options[:assets_to_check] [Array] - limit looking up balance to only assets in the array ["#{policy_id}#{asset_name}",...] (default: nil)
# @return [Set] Set of Hashes {"#{policy_id}#{asset_name}" => balance}
def assets_balance(assets, options = {})
  options[:delta] ||= 0
  assets_to_check = options[:assets_to_check]

  asset_set = assets.to_set { |x| { "#{x['policy_id']}#{x['asset_name']}" => x['quantity'] + options[:delta] } }
  if assets_to_check
    asset_set.select { |a| assets_to_check.include? a.keys.first }.to_set
  else
    asset_set
  end
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
    'assets_total' => assets_total }
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
    'assets_total' => assets_total }
end

##
# verify ADA balance on src and target wallets after transaction for amt ADA
# incurring fee ADA
def verify_ada_balance(src_after, src_before, target_after, target_before, amt, fee)
  expect(target_after['available']).to eq(amt + target_before['available'])
  expect(target_after['total']).to eq(amt + target_before['total'])

  expect(src_after['available']).to eq(src_before['available'] - amt - fee)
  expect(src_after['total']).to eq(src_before['total'] - amt - fee)
end

##
# Verify assets balance on target and src wallets after transaction
# @params src_after, src_before, target_after, target_before - src and target wallet balances before and after tx
# @param [Int] amt - amt of assets sent in tx
# @param [Array] assets_to_check - array of assets sent in the tx in the form of ["#{policy_id}#{asset_name}",...]
def verify_asset_balance(src_after, src_before, target_after, target_before, amt,
                         assets_to_check = ["#{ASSETS[0]['policy_id']}#{ASSETS[0]['asset_name']}",
                                            "#{ASSETS[1]['policy_id']}#{ASSETS[1]['asset_name']}"])

  target_total_after = assets_balance(target_after['assets_total'], { assets_to_check: assets_to_check })
  target_avail_after = assets_balance(target_after['assets_available'], { assets_to_check: assets_to_check })
  target_total_expected = assets_balance(target_before['assets_total'], { assets_to_check: assets_to_check, delta: +amt })
  target_avail_expected = assets_balance(target_before['assets_available'], { assets_to_check: assets_to_check, delta: +amt })
  src_total_after = assets_balance(src_after['assets_total'], { assets_to_check: assets_to_check })
  src_avail_after = assets_balance(src_after['assets_available'], { assets_to_check: assets_to_check })
  src_total_expected = assets_balance(src_before['assets_total'], { assets_to_check: assets_to_check, delta: -amt })
  src_avail_expected = assets_balance(src_before['assets_available'], { assets_to_check: assets_to_check, delta: -amt })

  if target_before['assets_total'] == []
    target_balance_expected = assets_to_check.to_set { |a| { a => amt } }
    expect(target_total_after).to eq target_balance_expected
    expect(target_avail_after).to eq target_balance_expected
  else
    expect(target_total_after).to eq target_total_expected
    expect(target_avail_after).to eq target_avail_expected
  end

  expect(src_total_after).to eq src_total_expected
  expect(src_avail_after).to eq src_avail_expected
end

def wait_for_tx_in_ledger(wid, tx_id, wallet_api = SHELLEY)
  eventually "Tx #{tx_id} is in ledger" do
    tx = wallet_api.transactions.get(wid, tx_id)
    tx.code == 200 && tx['status'] == 'in_ledger'
  end
end

def verify_tx_status(wid, tx_id, status, wallet_api = SHELLEY)
  tx = wallet_api.transactions.get(wid, tx_id)
  expect(tx.code).to eq(200)
  expect(tx['status']).to eq(status)
end

## Mint/burn helpers

# Build mint payload for construct tx
def mint(asset_name, quantity, policy_script, address = nil)
  mint = { 'operation' => { 'mint' => { 'quantity' => quantity } },
           'policy_script_template' => policy_script }
  mint['operation']['mint']['receiving_address'] = address unless address.nil?
  mint['asset_name'] = asset_name unless asset_name.nil?
  mint
end

# Build burn payload for construct tx
def burn(asset_name, quantity, policy_script)
  burn = { 'operation' => { 'burn' => { 'quantity' => quantity } },
           'policy_script_template' => policy_script }
  burn['asset_name'] = asset_name unless asset_name.nil?
  burn
end

##
# Gets assets list in the form of 'policy_id + asset_name' array
# @return [Array] - ["#{policy_id}#{asset_name}"...] of all minted/burnt assets
def get_assets_from_decode(tx_decoded_mint_or_burn)
  tx_decoded_mint_or_burn['tokens'].map do |x|
    assets = x['assets'].map { |z| z['asset_name'] }
    assets.map { |a| "#{x['policy_id']}#{a}" }
  end.flatten
end

def get_policy_id_from_decode(tx_decoded_mint_or_burn)
  tx_decoded_mint_or_burn['tokens'].first['policy_id']
end

##
# Balance -> Sign -> Submit
def balance_sign_submit(wid, payload)
  tx_balanced = SHELLEY.transactions.balance(wid, payload)
  expect(tx_balanced).to be_correct_and_respond 202

  tx_signed = SHELLEY.transactions.sign(wid, PASS, tx_balanced['transaction'])
  expect(tx_signed).to be_correct_and_respond 202

  tx_submitted = SHELLEY.transactions.submit(wid, tx_signed['transaction'])
  expect(tx_submitted).to be_correct_and_respond 202

  [tx_balanced, tx_signed, tx_submitted]
end

##
# Construct -> Sign -> Submit
def construct_sign_submit(wid,
                          payments = nil,
                          withdrawal = nil,
                          metadata = nil,
                          delegations = nil,
                          mint = nil,
                          validity_interval = nil)

  tx_constructed = SHELLEY.transactions.construct(wid,
                                                  payments,
                                                  withdrawal,
                                                  metadata,
                                                  delegations,
                                                  mint,
                                                  validity_interval)
  expect(tx_constructed).to be_correct_and_respond 202

  tx_signed = SHELLEY.transactions.sign(wid, PASS, tx_constructed['transaction'])
  expect(tx_signed).to be_correct_and_respond 202

  tx_submitted = SHELLEY.transactions.submit(wid, tx_signed['transaction'])
  expect(tx_submitted).to be_correct_and_respond 202

  [tx_constructed, tx_signed, tx_submitted]
end

def create_policy_key_if_not_exists(wid)
  gpkey = SHELLEY.keys.get_policy_key(wid)
  if gpkey.code == 403 && gpkey['code'] == 'missing_policy_public_key'
    pkey = SHELLEY.keys.create_policy_key(wid, PASS)
    expect(pkey).to be_correct_and_respond 202
    pkey
  else
    gpkey
  end
end

##
# Plutus helpers

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
# @see lib/wallet/extra/Plutus/FlatInteger.hs
#      reference Haskell impl of encoding Int into sequence of bits
# @see lib/wallet/src/Test/Integration/Plutus.hs
#      the way it's done in the integration tests
#      ```
#      idxEncoded = toHex $ BS.pack $ Bits.asBytes
#                  $ toBits "00" <> Bits.bits (fromIntegral idx :: Integer) <> toBits "001100"
#      ```
def plutus_encode_idx(int)
  raise 'Not supported index. (0-127) are supported.' if int > 127

  # convert int to binary and add trailing bit
  b = "#{int.to_s(2)}0"
  # add additional leading bits so it is 8-bit long
  b = ('0' * (8 - b.length)) + b
  # add additional leading and trailing bits
  b = "00#{b}001100"
  # convert to hex and add leading 0's if needed (so it is 4 digit long)
  h = binary_to_hex(b)
  ('0' * (4 - h.length)) + h
end

def get_simple_scripts_file_path(file)
  File.join(FIXTURES_DIR, 'simple', file)
end

def get_plutus_file_path(file)
  File.join(FIXTURES_DIR, 'plutus', file)
end

def get_plutus_tx(file)
  JSON.parse(File.read(get_plutus_file_path(file)))
end

def read_mustached_file(file, ctx = {})
  Mustache.render(File.read(get_plutus_file_path(file)), ctx).strip
end

def get_templated_plutus_tx(file, ctx = {})
  JSON.parse(read_mustached_file(file, ctx))
end

##
# Get policyId of base16-encoded minting policy
# which is Blake2b (28 byte long) hash of (script tag = 0x01 + policy)
def get_policy_id(policy)
  key = Blake2b::Key.none
  Blake2b.hex(hex_to_bytes("01#{policy}"), key, 28)
end

##
# Get all sent ADA amounts from the wallet from decoded tx outputs
# We assume multi output transaction
def get_sent_amts(outputs)
  outputs.map { |o| o['amount']['quantity'] if o['derivation_path'].nil? }
end

##
# The same as get_sent_amts, but we assume single output tx
def get_sent_amt(outputs)
  get_sent_amts(outputs).first
end
