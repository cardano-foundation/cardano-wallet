# frozen_string_literal: true

##
# secrets 🤫
# One needs to set these environment variables to run the tests
# on CI they are set in GitHub Secrets
# $ export TESTS_E2E_FIXTURES=... - for decrypting ./fixtures/fixture_wallets.json.gpg
# $ export BUILDKITE_API_TOKEN=... - for downloading artifacts from Buildkite

# file with mnemonics of fixture wallets
# decoded from ./fixtures/fixture_wallets.json.gpg using $TESTS_E2E_FIXTURES
ENV['TESTS_E2E_FIXTURES_FILE'] ||= './fixtures/fixture_wallets.json'

##
# Wallet/node databases, logs and configs will be stored here
ENV['TESTS_E2E_STATEDIR'] ||= './state'
ENV['CARDANO_NODE_CONFIGS'] ||= File.join(ENV.fetch('TESTS_E2E_STATEDIR', nil), 'configs')
ENV['TESTS_LOGDIR'] ||= File.join(ENV.fetch('TESTS_E2E_STATEDIR', nil), 'logs')
ENV['TESTS_NODE_DB'] ||= File.join(ENV.fetch('TESTS_E2E_STATEDIR', nil), 'node_db')
ENV['TESTS_WALLET_DB'] ||= File.join(ENV.fetch('TESTS_E2E_STATEDIR', nil), 'wallet_db')

##
# Wallet/node binaries will be downloaded here from Hydra.
# NOTE: Running `rake run_on[testnet,local]' overrides this and assumes node and wallet on $PATH
ENV['TESTS_E2E_BINDIR'] ||= './bins'

ENV['TESTS_E2E_TOKEN_METADATA'] ||= 'https://metadata.world.dev.cardano.org'
ENV['TESTS_E2E_SMASH'] ||= 'https://smash.shelley-qa.dev.cardano.org/'
ENV['WALLET_PORT'] ||= '8090'
ENV['NETWORK'] ||= 'preprod'

##
# Apply workaround for ADP-827 - Deleting wallets sometimes takes >60s in integration tests
ENV['CARDANO_WALLET_TEST_INTEGRATION'] = '1'
