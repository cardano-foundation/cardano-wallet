##
# fixture wallets with mnemonics
ENV['TESTS_E2E_FIXTURES'] ||= "this_is_wrong_secret"
ENV['TESTS_E2E_FIXTURES_FILE'] ||= "./fixtures/fixture_wallets.json"

##
# Wallet/node databases, logs and configs will be stored here
ENV['TESTS_E2E_STATEDIR'] ||= "./state"

##
# Wallet/node binaries will be downloaded here from Hydra.
# NOTE: Running `rake run_on[testnet,local]' overrides this and assumes node and wallet on $PATH
ENV['TESTS_E2E_BINDIR'] ||= "./bins"

ENV['TESTS_E2E_TOKEN_METADATA'] ||= "https://metadata.cardano-testnet.iohkdev.io/"
