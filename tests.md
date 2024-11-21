# System-level Tests

This document attempts to collect all system-level tests that are part of the cardano-wallet codebase in order to be able to consolidate testing assets into a coherent suite of tests. To this end, the plan is:

* Collect existing Haskell integration tests and Ruby end-to-end tests
* Identify tests in the latter that are surely covered by the former
* Identify gaps, eg. existing e2e tests that do not have a counterpart as integration tests.
  * For each of those gaps, decide whether or not we want to pay the cost of maintaining the test
  * If the answer is yes, port the test as an Integration Tests
* Identify system-level (integration) tests who could be implemented at a lower-level, eg. as unit or component level tests, not requiring a full blown process and cardano-node to run
  * Port thos tests as unit tests

This process is expected to take time. This document will serve as a central point to track progress.

## Categories of tests

By _System tests_ we mean tests that depend on running external processes, possibly even servers, to run. Our Haskell "integration" tests and Ruby E2E tests fall into this category as they both rely on runnning cardano-wallet process, and cardano-node process.

## Integration Tests

```
describe "SHELLEY_WALLETS"
      it "WALLETS_CREATE_01 - Create a wallet"
      it "WALLETS_CREATE_01.1 create a wallet restoring from tip"
  describe "OWASP_INJECTION_CREATE_WALLET_01 - \
      it "WALLETS_CREATE_02 - Restored wallet preserves funds"
      it "WALLETS_CREATE_03,09 - Cannot create wallet that exists"
  describe "WALLETS_CREATE_04 - Wallet name"
  describe "WALLETS_CREATE_05 - Mnemonics"
  describe "WALLETS_CREATE_06 - Mnemonics second factor"
  describe "WALLETS_CREATE_07 - Passphrase"
  describe "WALLETS_CREATE_08 - address_pool_gap"
      it "WALLETS_CREATE_08 - default address_pool_gap"
  describe "WALLETS_CREATE_09 - HTTP headers"
      it "WALLETS_CREATE_10 - Create wallet with one change address mode on"
      it "WALLETS_GET_01 - can get wallet details"
      it "WALLETS_GET_02, WALLETS_DELETE_01 - Deleted wallet is not available"
      it "WALLETS_LIST_01 - Created a wallet can be listed"
      it "WALLETS_LIST_01 - Wallets are listed from oldest to newest"
      it "WALLETS_LIST_02 - Deleted wallet not listed"
      it "WALLETS_UPDATE_01 - Updated wallet name is available"
  describe "WALLETS_UPDATE_02 - Various names"
      it "WALLETS_UPDATE_03 - Deleted wallet cannot be updated (404)"
  describe "WALLETS_UPDATE_04 - HTTP headers"
      it "WALLETS_UPDATE_PASS_01a - passphraseLastUpdate gets updated"
      it "WALLETS_UPDATE_PASS_01b - passphraseLastUpdate gets updated, mnemonic"
      it "WALLETS_UPDATE_PASS_01c - passphraseLastUpdate gets updated, mnemonic \
  describe "WALLETS_UPDATE_PASS_02 - New passphrase values"
      it "WALLETS_UPDATE_PASS_03 - Old passphrase incorrect"
      it "WALLETS_UPDATE_PASS_03 - Mnemonic incorrect"
  describe "WALLETS_UPDATE_PASS_03 - Can update pass from pass that's boundary\
      it "WALLETS_UPDATE_PASS_04 - Deleted wallet is not available"
      it "WALLETS_UPDATE_PASS_04 - Deleted wallet is not available, mnemonic"
  describe "WALLETS_UPDATE_PASS_05,06 - Transaction after updating passphrase"
  describe "WALLETS_UPDATE_PASS_07 - HTTP headers"
      it "WALLETS_UTXO_01 - Wallet's inactivity is reflected in utxo"
      it "WALLET_UTXO_SNAPSHOT_01 - \
      it "WALLET_UTXO_SNAPSHOT_02 - \
      it "WALLET_UTXO_SNAPSHOT_03 - \
      it "WALLETS_UTXO_02 - Sending and receiving funds updates wallet's utxo."
      it "WALLETS_UTXO_03 - Deleted wallet is not available for utxo"
  describe "WALLETS_UTXO_04 - HTTP headers"
      it "WALLETS_GET_KEY_01 - golden tests for verification key"
      it "WALLETS_GET_KEY_02 - invalid index for verification key"
      it "WALLETS_GET_KEY_03 - unknown wallet"
      it "WALLETS_SIGNATURES_01 - can verify signature"
      it "WALLETS_SIGNATURES_02 - invalid index for signing key"
      it "WALLETS_SIGNATURES_03 - unknown wallet"
      it "BYRON_WALLETS_UTXO -\
      it "BYRON_WALLETS_UPDATE_PASS -\
      it "BYRON_WALLETS_UPDATE -\
      it "BYRON_WALLETS_GET_02 - Byron ep does not show Shelley wallet"
      it "BYRON_WALLETS_GET_03 - Shelley ep does not show Byron wallet"
      it "BYRON_WALLETS_LIST_02,03 - \
      it "BYRON_WALLETS_LIST_04, DELETE_01 - \
      it "BYRON_WALLETS_DELETE_02 - Byron ep does not delete Shelley wallet"
      it "BYRON_WALLETS_DELETE_03 - Shelley ep does not delete Byron wallet"
      it "WALLETS_NETWORK_SHELLEY - Wallet has the same tip as network/information"

describe "No backend required"
        $ describe "Miscellaneous CLI tests" MiscellaneousCLI.spec
    describe "API Specifications" $ do
        parallel $ describe "CLI Specifications" $ do

spec = describe "COMMON_CLI_PORTS"
    it "PORT_01 - Can't reach server with wrong port (wallet list)"
    it "PORT_01 - Can't reach server with wrong port (wallet create)"
    it "PORT_01 - Can't reach server with wrong port (wallet get)"
    it "PORT_01 - Can't reach server with wrong port (wallet delete)"
    it "PORT_01 - Can't reach server with wrong port (wallet update)"
    it "PORT_01 - Can't reach server with wrong port (transction create)"
    it "PORT_01 - Can't reach server with wrong port (address list)"
  describe "PORT_04 - Fail nicely when port is out-of-bounds"
spec = describe "restoration of wallets"
/spec = describe "COMMON_CLI_NETWORK"
    it "CLI_NETWORK - cardano-wallet network information"
    it "NETWORK_PARAMS - network parameters"
    it "CLI_NETWORK - network clock"

spec = describe "BYRON_WALLETS"
    it "BYRON_GET_04, DELETE_01 - Deleted wallet is not available"
    it "BYRON_LIST_01 - Byron Wallets are listed from oldest to newest"
    it "BYRON_LIST_01 - Interleave of Icarus and Random wallets"
  describe "BYRON_RESTORE_01, GET_01, LIST_01 - Restore a wallet"
    it "BYRON_RESTORE_02 - One can restore previously deleted wallet"
    it "BYRON_RESTORE_03 - Cannot restore wallet that exists"
  describe "BYRON_RESTORE_06 - Passphrase"
    it "BYRON_UPDATE_NAME_01 - Update names of wallets"
    it "BYRON_UPDATE_NAME_02 - Update names of wallets from Xprv"
    it "BYRON_UTXO_01 - Wallet's inactivity is reflected in utxo"
    it "BYRON_WALLET_UTXO_SNAPSHOT_01 - \
    it "BYRON_WALLET_UTXO_SNAPSHOT_02 - \
    it "BYRON_WALLET_UTXO_SNAPSHOT_03 - \
    it "BYRON_UPDATE_PASS_01 - change passphrase"
    it "BYRON_UPDATE_PASS_02 - Old passphrase incorrect"
    it "BYRON_UPDATE_PASS_03 - Updating passphrase with no password wallets"
    it "BYRON_UPDATE_PASS_04a - Updating passphrase with no password wallets"
    it "BYRON_UPDATE_PASS_04b - Regression test"
    it "BYRON_UPDATE_PASS_07 - Updating passphrase with short password wallets"

spec = describe "BYRON_MIGRATIONS"
    it "BYRON_CREATE_MIGRATION_PLAN_01r - Can create a migration plan for a random wallet."
    it "BYRON_CREATE_MIGRATION_PLAN_01i - Can create a migration plan for an Icarus wallet."
    it "BYRON_CREATE_MIGRATION_PLAN_02r - Cannot create plan for empty wallet."
    it "BYRON_CREATE_MIGRATION_PLAN_02i - Cannot create plan for empty wallet."
    it "BYRON_CREATE_MIGRATION_PLAN_03 - Cannot create plan for Shelley wallet using Byron endpoint."
    it "BYRON_CREATE_MIGRATION_PLAN_04 - Cannot create a plan for a wallet that only contains dust."
    it "BYRON_CREATE_MIGRATION_PLAN_05r - Creating a plan is deterministic."
    it "BYRON_CREATE_MIGRATION_PLAN_05i - Creating a plan is deterministic."
  describe "BYRON_MIGRATE_01 - \After a migration operation successfully completes, the correct amounts eventually become available in the target wallet for an arbitrary number of specified addresses, and the balance of the source wallet is completely depleted."
    it "BYRON_MIGRATE_02 - Can migrate a large wallet requiring more than one transaction."
    it "BYRON_MIGRATE_03 - Migrating an empty wallet should fail."
    it "BYRON_MIGRATE_04 - Actual fee for migration is identical to predicted fee."
    it "BYRON_MIGRATE_05 - Migration fails if the wrong passphrase is supplied."
  describe "BYRON_MIGRATE_06 - It's possible to migrate to any valid address."
    it "BYRON_MIGRATE_07 - Including an invalidly-formatted passphrase results in a parser error."
    it "BYRON_MIGRATE_08 - It's not possible to migrate a wallet whose total balance is less than the minimum ada quantity for an output."

spec = describe "BYRON_CLI_WALLETS"
  describe "CLI_BYRON_GET_04, CLI_BYRON_DELETE_01, BYRON_RESTORE_02, BYRON_RESTORE_03 -\
  describe "CLI_BYRON_RESTORE_01, CLI_BYRON_GET_01, CLI_BYRON_LIST_01 -\
  describe "CLI_BYRON_RESTORE_06 - Passphrase"
      it "CLI_BYRON_UPDATE_NAME_01 - Update names of wallets"
      it "CLI_BYRON_UPDATE_NAME_02 - When updated name too long"
      it "CLI_BYRON_UTXO_01 - Wallet's inactivity is reflected in utxo"
      it "CLI_BYRON_UPDATE_PASS_01 - change passphrase"
      it "CLI_BYRON_UPDATE_PASS_02 - Old passphrase incorrect"
  describe "CLI_BYRON_UPDATE_PASS_03 - Pass length incorrect"

spec = describe "NEW_SHELLEY_TRANSACTIONS"
    it "TRANS_NEW_CREATE_01a - Empty payload is not allowed"
    it "TRANS_NEW_CREATE_01b - Validity interval only is not allowed"
    it "TRANS_NEW_CREATE_01c - No payload is bad request"
    it "TRANS_NEW_CREATE_02a - Only metadata"
    it "TRANS_NEW_CREATE_02b - Only metadata, untyped"
    it "TRANS_NEW_CREATE_04ab - Constructed inputs = Decoded inputs"
    it "TRANS_NEW_CREATE_04b - Cannot spend less than minUTxOValue"
    it "TRANS_NEW_CREATE_04c - Can't cover fee"
    it "TRANS_NEW_CREATE_04d - Not enough money"
    it "TRANS_NEW_CREATE_04d - No UTxOs available"
    it "TRANS_NEW_CREATE_04e- Multiple Output Tx to single wallet"
    it "TRANS_NEW_ASSETS_CREATE_01a - Multi-asset tx with Ada"
    it "TRANS_NEW_ASSETS_CREATE_01b - Multi-asset tx with not enough Ada"
    it "TRANS_NEW_ASSETS_CREATE_01c - Multi-asset tx without Ada"
    it "TRANS_NEW_ASSETS_CREATE_02 - using reference script"
    it "TRANS_NEW_BALANCE_02a - Cannot balance on empty wallet"
    it "TRANS_NEW_BALANCE_02b - Cannot balance when I cannot afford fee"
    it "TRANS_NEW_SIGN_01 - Sign single-output transaction"
    it "TRANS_NEW_SIGN_02 - Rejects unsigned transaction"
    it "TRANS_NEW_SIGN_03 - Sign withdrawals"
    it "TRANS_NEW_SIGN_04 - Sign extra required signatures"
  describe
  describe
    it "TRANS_NEW_SUBMIT_03 - Can submit transaction encoded in base16"
    xdescribe "Plutus scenarios"
    it "TRANS_NEW_SUBMIT_04 - Mary and Babbage foreign txs submitted"
    it "TRANS_NEW_CREATE_10l - Minting when assetName too long"
    it "TRANS_NEW_CREATE_10m1 - Minting amount too big"
    it "TRANS_NEW_CREATE_10m2 - Minting amount = 0"
    it "TRANS_NEW_CREATE_10d - Minting assets without timelock"
    it "TRANS_NEW_CREATE_10f - Burning assets without timelock"
  describe
  describe
    it "TRANS_NEW_CREATE_12 - Cannot vote in Babbage"
    it "TRANS_NEW_LIST_05 - filter address output side"
    it "TRANS_NEW_LIST_06 - filter address input side"

spec = describe "SHELLEY_SETTINGS"
    it "SETTINGS_01 - Can put and read settings"
    it "SETTINGS_02 - Changing pool_metadata_source re-syncs metadata"

spec = describe "SHELLEY_HW_WALLETS"
    it "HW_WALLETS_01 - Restoration from account public key preserves funds"
  describe "HW_WALLETS_03 - Cannot do operations requiring private key"
        it "Cannot send tx"
        it "Cannot update pass"
  describe "HW_WALLETS_04 - Can manage HW wallet the same way as others"
        it "Can update name"
        it "Can get tx fee"
        it "Can delete"
        it "Can see utxo"
        it "Can list addresses"
        it "Can have address pool gap"
        it "Can list transactions"
        it "Can create a coin selection"
  describe "HW_WALLETS_05 - Wallet from pubKey is available"
        it "Can get wallet"
        it "Can list wallet"
        it "The same account and mnemonic wallet can live side-by-side"

spec = describe "SHELLEY_NETWORK"
    it "NETWORK_PARAMS - Able to fetch network parameters"

spec = describe "WITHDRAWALS"
    it "TRANS_NEW_CREATE_03a - Withdrawal from self, 0 rewards"
    it "TRANS_NEW_CREATE_03a - Withdrawal from self"

spec = describe "BYRON_NETWORK"
    it "NETWORK_PARAMS - Able to fetch network parameters"

spec = describe "SHELLEY_CLI_WALLETS"
    it "BYRON_GET_03 - Shelley CLI does not show Byron wallet"
    it "BYRON_LIST_03 - Shelley CLI does not list Byron wallet"
    it "BYRON_DELETE_03 - Shelley CLI does not delete Byron wallet"
    it "BYRON_WALLETS_UTXO -\
    it "BYRON_WALLETS_UPDATE_PASS -\
    it "BYRON_WALLETS_UPDATE -\
    it "WALLETS_CREATE_01,08 - Can create a wallet"
    it "WALLETS_CREATE_02 - Restored wallet preserves funds"
    it "WALLETS_CREATE_03 - Cannot create wallet that exists"
  describe "WALLETS_CREATE_04 - Wallet names"
  describe "WALLETS_CREATE_04 - Wallet names invalid"
  describe "WALLETS_CREATE_05 - Can create wallet with different mnemonic sizes"
  describe "WALLETS_CREATE_05 - Can't create wallet with wrong size of mnemonic"
  describe "WALLETS_CREATE_06 - Can create wallet with different mnemonic snd factor sizes"
  describe "WALLETS_CREATE_06 - Can't create wallet with wrong size of mnemonic snd factor"
  describe "WALLETS_CREATE_07 - Passphrase is valid"
  describe "WALLETS_CREATE_07 - When passphrase is invalid"
  describe "WALLETS_CREATE_08 - --address-pool-gap values"
    it "WALLETS_GET_01 - Can get a wallet"
  describe "WALLETS_GET_03,04 - Cannot get wallets with false ids"
    it "WALLETS_LIST_01 - Can list wallets"
    it "WALLETS_LIST_01 - Wallets are listed from oldest to newest"
  describe "WALLETS_UPDATE_01,02 - Can update wallet name"
    it "WALLETS_UPDATE_PASS_01 - Can update passphrase normally"
    it "WALLETS_UPDATE_PASS_01 - Can update passphrase normally, mnemonic"
  describe "WALLETS_UPDATE_PASS_02 - New passphrase values"
    it "WALLETS_UPDATE_PASS_02 - \
  describe "WALLETS_UPDATE_PASS_03 - Old passphrase values"
  describe "WALLETS_UPDATE_PASS_03 - \
  describe "WALLETS_UPDATE_PASS_04 - Cannot update pass of wallets with false ids"
  describe "WALLETS_UPDATE_PASS_05,06 - \
    it "WALLETS_DELETE_01, WALLETS_LIST_02 - Can delete wallet"
    it "WALLETS_UTXO_01 - Wallet's inactivity is reflected in utxo"
    it "WALLET_UTXO_SNAPSHOT_01 - \
    it "WALLETS_UTXO_02 - Utxo statistics works properly"
  describe "WALLETS_UTXO_03 - non-existing wallets"
    it "WALLETS_UTXO_03 - Deleted wallet is not available for utxo"
    it "WALLETS_UTXO_03 - 'almost' valid walletId"

  describe "BYRON_CLI_ADDRESSES"
    describe "CLI_ADDRESS_CREATE_07 - False indexes"

spec = describe "SHELLEY_TRANSACTIONS"
    it "Regression ADP-626 - Filtering transactions between eras" $ do
    it "TRANS_CREATE_01x - Single Output Transaction"
    it "TRANS_CREATE_02x - Multiple Output Tx to single wallet"
    it "TRANS_CREATE_03 - 0 balance after transaction"
    it "TRANS_CREATE_04 - Can't cover fee"
    it "TRANS_CREATE_04 - Not enough money"
    it "TRANS_CREATE_04 - Wrong password"
    it "TRANS_CREATE_07 - Deleted wallet"
  describe "TRANS_CREATE_08 - Bad payload"
    it "TRANS_ASSETS_CREATE_01 - Multi-asset balance"
    it "TRANS_ASSETS_CREATE_01a - Multi-asset transaction with Ada"
    it "TRANS_ASSETS_CREATE_02a - Multi-asset transaction without Ada"
    it "TRANS_ASSETS_CREATE_02c - Send SeaHorses"
    it "TRANS_ASSETS_CREATE_02b - Multi-asset tx history"
    it "TRANS_ASSETS_LIST_01 - Asset list present"
    it "TRANS_ASSETS_LIST_02 - Asset list present when not used"
    it "TRANS_ASSETS_LIST_02a - Asset list present when not used"
    it "TRANS_ASSETS_GET_01 - Asset list present"
    it "TRANS_ASSETS_GET_02 - Asset not present when isn't associated"
    it "TRANS_ASSETS_GET_02a - Asset not present when isn't associated"
    it "TRANS_TTL_04 - Large TTL"
  describe "TRANSMETA_CREATE_01 - Including metadata within transactions"
    it "TRANSMETA_CREATE_03 - Transaction with too much metadata"
    it "TRANSMETA_ESTIMATE_03 - fee estimation with too much metadata"
  describe "TRANS_ESTIMATE_08 - Bad payload"
    it "TRANS_ESTIMATE_03a - we see result when we can't cover fee"
    it "TRANS_ESTIMATE_04 - Not enough money"
    it "TRANS_ESTIMATE_07 - Deleted wallet"
    it "TRANS_LIST_01 - Can list Incoming and Outgoing transactions"
    it "TRANS_LIST_02,03x - Can limit/order results with start, end and order"
  describe "TRANS_LIST_02,03 - Faulty start, end, order values"
    it "TRANS_LIST_02 - Start time shouldn't be later than end time"
    it "TRANS_LIST_03 - Minimum withdrawal shouldn't be 0"
    it "TRANS_LIST_04 - Deleted wallet"
    it "TRANS_GET_01 - Can get Incoming and Outgoing transaction"
    it "TRANS_GET_02 - Deleted wallet"
    it "TRANS_GET_03 - Using wrong transaction id"
    it "TRANS_GET_04 - Sumbitted transactions result in pending state"
  describe "TRANS_DELETE_03 - checking no transaction id error for "
  describe
    it "SHELLEY_TX_REDEEM_01 - Can redeem rewards from self"
    it "SHELLEY_TX_REDEEM_02 - Can redeem rewards from other"
    it "SHELLEY_TX_REDEEM_03 - Can't redeem rewards from other if none left"
    it "SHELLEY_TX_REDEEM_04 - Can always ask for self redemption"
    it "SHELLEY_TX_REDEEM_05 - Can't redeem rewards from unknown key"
    it "SHELLEY_TX_REDEEM_06 - Can't redeem rewards using byron wallet"
    it "SHELLEY_TX_REDEEM_06a - Can't redeem rewards if utxo = 0 from other"
    it "SHELLEY_TX_REDEEM_06b - Can't redeem rewards if utxo = 0 from self"
    it "SHELLEY_TX_REDEEM_07b - Can't redeem rewards if not enough money"

spec = describe "SHELLEY_CLI_TRANSACTIONS"
    it "TRANS_CREATE_01 - Can create transaction via CLI"
    it "TRANS_CREATE_02 - Multiple Output Tx to single wallet via CLI"
    it "TRANS_CREATE_04 - Wrong password"
  describe "TRANS_CREATE_05 - Invalid addresses"
  describe "TRANS_CREATE_06 - Invalid amount"
  describe "TRANS_CREATE_07 - False wallet ids"
    it "TRANS_CREATE_07 - 'almost' valid walletId"
    it "TRANS_CREATE_07 - Deleted wallet"
    it "TRANSMETA_CREATE_01a - \
    it "TRANSMETA_CREATE_01b - \
    it "TRANSTTL_CREATE_01 - Transaction with TTL via CLI"
  describe "TRANS_ESTIMATE_08 - Invalid addresses"
  describe "TRANS_ESTIMATE_09 - Invalid amount"
  describe "TRANS_LIST_01 - Listing transactions for an empty wallet"
    it "TRANS_LIST_01 - Can list Incoming and Outgoing transactions"
  describe "TRANS_LIST_02 - Start time shouldn't be later than end time"
    it "TRANS_LIST_03 - Can order results"
  describe "TRANS_LIST_02,03 - Faulty start, end, order values"
    it "TRANS_LIST_04 - 'almost' valid walletId"
    it "TRANS_LIST_04 - Deleted wallet"
  describe "TRANS_LIST_04 - False wallet ids"
    it "TRANS_LIST_RANGE_01 - \
    it "TRANS_LIST_RANGE_02 - \
    it "TRANS_LIST_RANGE_03 - \
    it "TRANS_GET_01 - Can get Incoming and Outgoing transaction"
    it "TRANS_GET_02 - Deleted wallet"
    it "TRANS_GET_03 - Using wrong transaction id"
    it "TRANS_DELETE_01 - Cannot forget pending transaction when not pending anymore via CLI"
    it "TRANS_DELETE_03 - Cannot forget tx that is not found via CLI"
  describe "TRANS_DELETE_04 - False wallet ids via CLI"
    it "TRANS_DELETE_06 -\
  describe "TRANS_DELETE_07 - invalid tx id via CLI"
    it "BYRON_TX_LIST_03 -\
    it "BYRON_TRANS_DELETE -\
  describe "BYRON_TRANS_CREATE / BYRON_TRANS_ESTIMATE -\

spec = describe "SHELLEY_COIN_SELECTION"
  describe "WALLETS_COIN_SELECTION_04 - HTTP headers"
    it "WALLETS_COIN_SELECTION_05a - can include metadata"
    it "WALLETS_COIN_SELECTION_05b - choke on invalid metadata"
    it "WALLETS_COIN_SELECTION_06a - can redeem rewards from self"
    it "WALLETS_COIN_SELECTION_06b - can redeem rewards from other"

spec = describe "BYRON_TRANSACTIONS"
  describe "BYRON_TRANS_ASSETS_CREATE_01 - Multi-asset transaction with ADA"
  describe "BYRON_TRANS_ASSETS_CREATE_02 - Multi-asset transaction with too little ADA"
  describe "BYRON_TRANS_ASSETS_CREATE_02a - Multi-asset transaction with no ADA"
  describe "BYRON_TRANS_ASSETS_LIST_01 - Asset list present"
  describe "BYRON_TRANS_ASSETS_LIST_02 - Asset list present when not used"
  describe "BYRON_TRANS_ASSETS_GET_01 - Asset list present"
  describe "BYRON_TRANS_ASSETS_GET_02 - Asset not present when isn't associated"
  describe "BYRON_TRANS_ASSETS_GET_02a - Asset not present when isn't associated"
  describe "BYRON_TRANS_CREATE_01 - Single Output Transaction Byron -> Shelley"
  describe "BYRON_TRANS_CREATE_01a - Single Output Transaction Byron -> Byron"
    it "BYRON_TRANS_CREATE_02 -\
    it "BYRON_TRANS_DELETE -\
    it "BYRON_TRANS_ESTIMATE -\
    it "BYRON_TX_LIST_02 -\
    it "BYRON_TX_LIST_03 -\
    it "BYRON_RESTORE_09 - Ledger wallet"
    it "BYRON_TX_LIST_01 - 0 txs on empty Byron wallet"
    it "BYRON_TX_LIST_01 - Can list transactions on Byron Wallet"
    it "BYRON_TX_LIST_01 - Can list transactions on Icarus Wallet"
  describe "BYRON_TX_LIST_LIMIT - Transactions can be limited"
  describe "BYRON_TX_LIST_01 - Faulty start, end, order values"
    it "BYRON_TX_LIST_01 - Start time shouldn't be later than end time"
    it "BYRON_TX_LIST_04 - Deleted wallet"
  describe "BYRON_TX_LIST_ADDRESS - Transactions can be filtered by address"

spec = describe "SHARED_WALLETS"
    it "SHARED_WALLETS_CREATE_01 - Create an active shared wallet from root xprv"
    it "SHARED_WALLETS_CREATE_01 - Compare wallet ids"
    it "SHARED_WALLETS_CREATE_01 - golden test comparing wallet id"
    it "SHARED_WALLETS_CREATE_02 - Create a pending shared wallet from root xprv"
    it "SHARED_WALLETS_CREATE_03 - Create an active shared wallet from account xpub"
    it "SHARED_WALLETS_CREATE_04 - Create a pending shared wallet from account xpub"
    it "SHARED_WALLETS_CREATE_05 - Create an active shared wallet from root xprv with self"
    it "SHARED_WALLETS_CREATE_06 - Create an active shared wallet from account xpub with self"
    it "SHARED_WALLETS_CREATE_07 - Incorrect script template due to NoCosignerInScript"
    it "SHARED_WALLETS_CREATE_08 - Incorrect script template due to UnknownCosigner"
    it "SHARED_WALLETS_CREATE_09 - Incorrect script template due to DuplicateXPub"
    it "SHARED_WALLETS_CREATE_10 - when recommended validation"
    it "SHARED_WALLETS_CREATE_11 - Correct script template when required validation"
    it "SHARED_WALLETS_CREATE_12 - Incorrect script template due to WrongScript - timelocks"
    it "SHARED_WALLETS_CREATE_13 - Incorrect account index"
    it "SHARED_WALLETS_CREATE_14 - Create wallet with one change address mode on"
    it "SHARED_WALLETS_DELETE_01 - Delete of a shared wallet"
    it "SHARED_WALLETS_PATCH_01 - active shared wallet"
    it "SHARED_WALLETS_PATCH_02 - Add cosigner for delegation script template"
    it "SHARED_WALLETS_PATCH_03 - Cannot add cosigner key in an active shared wallet"
    it "SHARED_WALLETS_PATCH_04 - add already existent key to other cosigner"
    it "SHARED_WALLETS_PATCH_05 - key in template"
    it "SHARED_WALLETS_PATCH_06 - payment one"
    it "SHARED_WALLETS_PATCH_07 - shared wallet's account key"
    it "SHARED_WALLETS_KEYS_01 - Getting verification keys works for active shared wallet"
    it "SHARED_WALLETS_KEYS_02 - Getting verification keys works for pending shared wallet"
    it "SHARED_WALLETS_LIST_01 - Created a wallet can be listed"
    it "SHARED_WALLETS_LIST_01 - Wallets are listed from oldest to newest"
    it "SHARED_WALLETS_LIST_02 - Deleted wallet not listed"
    it "SHARED_WALLETS_DISCOVER_01 - "
    it "SHARED_WALLETS_UTXO_01 -"
    it "SHARED_WALLETS_UTXO_02 - Sending and receiving funds updates "
    it "SHARED_WALLETS_UTXO_03 - Deleted wallet is not available ctx -> runResourceT
  describe "SHARED_WALLETS_UTXO_04 - HTTP headers"
    it "SHARED_WALLETS_UTXO_SNAPSHOT_01 - Can generate UTxO snapshot of empty wallet"
    it "SHARED_WALLETS_UTXO_SNAPSHOT_02 - Can generate UTxO snapshot of pure-ada wallet"

spec = describe "SHELLEY_MIGRATIONS"
  describe
  describe

spec = describe "SHELLEY_CLI_ADDRESSES"
    it "ADDRESS_LIST_01 - Can list addresses - default poolGap"
    it "ADDRESS_LIST_01 - Can list addresses - non-default poolGap"
    it "ADDRESS_LIST_02 - Can filter used and unused addresses"
    it "ADDRESS_LIST_02 - Shows nothing when there are no used addresses"
  describe "ADDRESS_LIST_02 - Invalid filters show error message"
    it "ADDRESS_LIST_03 - Generates new address pool gap"
  describe "ADDRESS_LIST_04 - False wallet ids"
    it "ADDRESS_LIST_04 - 'almost' valid walletId"
    it "ADDRESS_LIST_04 - Deleted wallet"
    it "BYRON_ADDRESS_LIST - Byron wallet on Shelley CLI"

spec = describe "SHELLEY_STAKE_POOLS"
    it "STAKE_POOLS_JOIN_01 - Cannot join non-existent wallet"
    it "STAKE_POOLS_JOIN_01 - Cannot join non-existent stakepool"
    it "STAKE_POOLS_JOIN_02 - \
    it "STAKE_POOLS_JOIN_02 - \
    it "STAKE_POOLS_JOIN_02a - \
    it "STAKE_POOLS_JOIN_02b - \
    it "STAKE_POOLS_JOIN_03 - Cannot join a pool that has retired"
    it "STAKE_POOLS_JOIN_EMPTY - Empty wallet cannot join a pool"
    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit"
    it "STAKE_POOLS_QUIT_03 - Can quit with rewards"
    it "STAKE_POOLS_JOIN_01 - Can rejoin another stakepool"
    it "STAKE_POOLS_JOIN_04 - Rewards accumulate"
  describe "STAKE_POOLS_JOIN_UNSIGNED_01"
        it "Can join a pool that's not retiring"
  describe "STAKE_POOLS_JOIN_UNSIGNED_02"
        $ it "Can join a pool that's retiring"
  describe "STAKE_POOLS_JOIN_UNSIGNED_03"
        $ it "Cannot join a pool that's retired"
  describe "STAKE_POOLS_JOIN_UNSIGNED_04"
        $ it "Cannot join a pool that's never existed"
  describe "STAKE_POOLS_QUIT_UNSIGNED_02"
        $ it "Cannot quit if not delegating"
  describe "STAKE_POOLS_JOIN_01x - Fee boundary values"
  describe "STAKE_POOLS_QUIT_01x - Fee boundary values"
    it "STAKE_POOLS_ESTIMATE_FEE_01 - can estimate fees"
  describe "STAKE_POOLS_LIST_01 - List stake pools"
        it "has non-zero saturation & stake"
        it "pools have the correct retirement information"
        it "eventually has correct margin, cost and pledge"
        it "at least one pool eventually produces block"
        it "contains pool metadata"
        it "contains and is sorted by non-myopic-rewards"
        it "non-myopic-rewards are based on stake"
    it "STAKE_POOLS_LIST_05 - Fails without query parameter"
    it "STAKE_POOLS_SMASH_01 - fetching metadata from SMASH works with delisted pools"
    it "STAKE_POOLS_SMASH_HEALTH_01 - Can check SMASH health when configured"
  describe "STAKE_POOLS_SMASH_HEALTH_02 - Cannot check SMASH health when not configured"
    it "STAKE_POOLS_SMASH_HEALTH_03 - Can check SMASH health via url"
  describe "STAKE_POOLS_SMASH_HEALTH_04 - SMASH url needs to be valid"
    it "STAKE_KEY_LIST_01 - Can list stake keys"
    it "STAKE_KEY_LIST_02 - Can list foreign stake key from UTxO"

spec = describe "SHELLEY_ADDRESSES"
    it "BYRON_ADDRESS_LIST - Byron wallet on Shelley ep"
    it "ADDRESS_LIST_01 - Can list known addresses on a default wallet"
    it "ADDRESS_LIST_01 - Can list addresses with non-default pool gap"
    it "ADDRESS_LIST_02 - Can filter used and unused addresses"
    it "ADDRESS_LIST_02 - Shows nothing when there are no used addresses"
  describe "ADDRESS_LIST_02 - Invalid filters are bad requests"
    it "ADDRESS_LIST_03 - Generates new address pool gap"
    it "ADDRESS_LIST_04 - Deleted wallet"
    it "ADDRESS_LIST_05 - bech32 HRP is correct - testnet"
    it "ADDRESS_LIST_06 - Used change addresses are listed after a transaction \
    it "ADDRESS_INSPECT_01 - Address inspect OK Icarus"
    it "ADDRESS_INSPECT_02 - Address inspect OK Byron"
    it "ADDRESS_INSPECT_03 - Address inspect OK reward"
    it "ADDRESS_INSPECT_04 - Address inspect KO"
    it "ADDRESS_INSPECT_05 - Address inspect OK bech32"
    it "ANY_ADDRESS_POST_01 - Golden tests for enterprise script address - signature"
    it "ANY_ADDRESS_POST_02 - Golden tests for enterprise script address - any"
    it "ANY_ADDRESS_POST_03 - Golden tests for enterprise script address - all"
    it "ANY_ADDRESS_POST_04 - Golden tests for enterprise script address - some"
    it "ANY_ADDRESS_POST_05 - Golden tests for reward account script address - any"
    it "ANY_ADDRESS_POST_06 - Golden tests for delegating script address - any"
    it "ANY_ADDRESS_POST_07a - Golden tests for enterprise address - from non-extended public key"
    it "ANY_ADDRESS_POST_07b - Golden tests for enterprise address - from extended public key"
    it "ANY_ADDRESS_POST_07c - Golden tests for enterprise address - from key hash"
    it "ANY_ADDRESS_POST_08a - Golden tests for reward account address - from non-extended public key"
    it "ANY_ADDRESS_POST_08b - Golden tests for reward account address - from extended public key"
    it "ANY_ADDRESS_POST_08c - Golden tests for reward account address - from key hash"
    it "ANY_ADDRESS_POST_09a - Golden tests for delegating address with both non-extended pub key credentials"
    it "ANY_ADDRESS_POST_09b - Golden tests for delegating address with both extended pub key credentials"
    it "ANY_ADDRESS_POST_09c - Golden tests for delegating address with both key hash credentials"
    it "ANY_ADDRESS_POST_09d - Golden tests for delegating address with mixed credentials"
    it "ANY_ADDRESS_POST_09e - Golden tests for delegating address with mixed credentials"
    it "ANY_ADDRESS_POST_09f - Golden tests for delegating address with mixed credentials"
    it "ANY_ADDRESS_POST_10 - Golden tests for delegating address - payment from script, stake from key"
    it "ANY_ADDRESS_POST_11 - Golden tests for delegating address - payment from key, stake from script"
    it "ANY_ADDRESS_POST_12 - Delegating addresses API roundtrip"
    it "ANY_ADDRESS_POST_13 - Golden tests for script with timelocks"
    it "ANY_ADDRESS_POST_14a - at_least 0 is valid when non-validated"
    it "ANY_ADDRESS_POST_14b - at_least 0 is valid when validation is required"
    it "ANY_ADDRESS_POST_14c - at_least 0 is not valid when validation is recommended"
    it "ANY_ADDRESS_POST_15a - at_least 4 is valid when non-validated"
    it "ANY_ADDRESS_POST_15b - at_least 4 is valid when validation is required"
    it "ANY_ADDRESS_POST_15c - at_least 4 is not valid when validation is recommended"
    it "ANY_ADDRESS_POST_16a - script with duplicated verification keys is valid when non-validated"
    it "ANY_ADDRESS_POST_16b - script with duplicated verification keys is valid when required validation used"
    it "ANY_ADDRESS_POST_16c - script with duplicated verification keys is invalid when recommended validation used"
    it "ANY_ADDRESS_POST_17a - Script with contradictory timelocks is valid when validation not used"
    it "ANY_ADDRESS_POST_17b - Script with contradictory timelocks is invalid when required validation is used"
    it "ANY_ADDRESS_POST_17c - Script with contradictory timelocks is invalid when recommended validation is used"
    it "ANY_ADDRESS_POST_17d - script with mixed payment/delegation verification keys is invalid"
    it "POST_ACCOUNT_01 - Can retrieve account public keys"
    it "POST_ACCOUNT_02 - Can get account public key using purpose"
    it "ANY_ADDRESS_POST_15 - Staking address using stake credential non-hashed"
    it "ANY_ADDRESS_POST_16 - Staking address using stake credential hashed"

spec = describe "BLOCKS"
    it "LATEST_BLOCK Current tip is reported"

spec = describe "VOTING_TRANSACTIONS"
    it "VOTING_01a - Can vote and revote and then delegate"
    it "VOTING_01b - Can vote and revote after delegation"
    it "VOTING_01c - Can vote together with delegation"
    it "VOTING_01d - Can joinStakePool and quitStakePool"
    it "VOTING_01e - Cannot vote in Babbage"
    it "VOTING_01f - Voting works in Conway in absence of pool delegation"
    it "VOTING_01g - Voting works in Conway in presence of pool delegation"
    it "VOTING_01h - Delegation works in Conway in presence of voting and does not change it"
    it "VOTING_01i - Delegation works in Conway in presence of voting and does not change it"
    it "VOTING_01j - Delegation works in Conway in presence of voting after wallet delations and restoration"

spec = describe "SHARED_TRANSACTIONS"
    it "SHARED_TRANSACTIONS_CREATE_01a -Empty payload is not allowed"
    it "SHARED_TRANSACTIONS_CREATE_04d - Can't cover fee"
  describe
    it "SHARED_TRANSACTIONS_LIST_03 - Minimum withdrawal shouldn't be 0"
    it "SHARED_TRANSACTIONS_LIST_04 - Deleted wallet"
    it "SHARED_TRANSACTIONS_GET_02 - Deleted wallet"
    it "SHARED_TRANSACTIONS_GET_03 - Using wrong transaction id"
    it "SHARED_TRANSACTIONS_LIST_05 - filter address output side"
    it "SHARED_TRANSACTIONS_LIST_06 - filter address input side"

spec = describe "BYRON_COIN_SELECTION"
    it "BYRON_COIN_SELECTION_00 - No coin selection on Byron random"

spec = describe "SHARED_ADDRESSES"
    it "SHARED_ADDRESSES_LIST_01 - Can list known addresses on a default wallet"
    it "SHARED_ADDRESSES_LIST_02 - Can list known addresses on a pending wallet"

  describe "BYRON_ADDRESSES"

spec = describe "SHELLEY_CLI_HW_WALLETS"
    it "HW_WALLETS_01x - Restoration from account public key preserves funds"
  describe "HW_WALLETS_03 - Cannot do operations requiring private key"
        it "Cannot send tx"
        it "Cannot update pass"
  describe "HW_WALLETS_04 - Can manage HW wallet the same way as others"
        it "Can update name"
        it "Can get tx fee"
        it "Can delete"
        it "Can see utxo"
        it "Can list addresses"
        it "Can have address pool gap"
        it "Can list transactions"
  describe "HW_WALLETS_05 - Wallet from pubKey is available"
        it "The same account and mnemonic wallet can live side-by-side"
  describe "HW_WALLETS_06 - Test parameters"
    describe "Wallet names valid"
    describe "Wallet names invalid"
    describe "Pub Key invalid"
    describe "Address pool gap invalid"

spec = describe "BYRON_HW_WALLETS"
    it "HW_WALLETS_01 - Restoration from account public key preserves funds"
  describe "HW_WALLETS_03 - Cannot do operations requiring private key"
        it "Cannot send tx"
        it "Cannot update pass"
  describe "HW_WALLETS_04 - Can manage HW wallet the same way as others"
        it "Can update name"
        it "Can get tx fee"
        it "Can delete"
        it "Can see utxo"
        it "Can list addresses"
        it "Can have address pool gap"
        it "Can list transactions"
        it "Can get coin selection"
  describe "HW_WALLETS_05 - Wallet from pubKey is available"
        it "Can get wallet"
        it "Can list wallet"
        it "The same account and mnemonic wallet can live side-by-side"

spec = describe "NEW_SHELLEY_TRANSACTIONS"
    it "TRANS_NEW_JOIN_01b - Invalid pool id"
    it "TRANS_NEW_JOIN_01b - Absent pool id"
    it "TRANS_NEW_JOIN_01c - Multidelegation not supported"
    it "TRANS_NEW_JOIN_01d - Multiaccount not supported"
    it "TRANS_NEW_JOIN_01e - Can re-join and withdraw at once"
    it "TRANS_NEW_JOIN_01f - Cannot re-join the same pool in Babbage"
    it "TRANS_NEW_JOIN_01f - Can re-join the same pool in Conway"
    it "TRANS_NEW_QUIT_01 - Cannot quit if not joined"
```

## E2E Tests

the following tests are actually not about cardano-wallet at all but testing other utilities: [cardano-address]()
They also depend on code defined in [cardano-wallet-rb](https://github.com/piotr-iohk/cardano-wallet-rb)

**They can be safely discarded**

```
misc:
     describe CardanoWallet::Misc::Node do
       it 'Can get latest block header info' do
     describe CardanoWallet::Misc::Network do
       it 'Can get network information' do
       it 'Can check network clock offset' do
       it 'Can check network parameters' do
         it 'SMASH health - unreachable' do
         it 'SMASH health - bad url' do
       it 'Inspect invalid address' do
       it 'Inspect Shelley payment address' do
       it 'Inspect Shelley stake address' do
       it 'Inspect Byron Random address' do
       it 'Inspect Byron Icarus address' do
         it 'Enterprise script address - signature' do
         it 'Enterprise script address - any' do
         it 'Enterprise script address - all' do
         it 'Enterprise script address - some' do
         it 'Reward account script address - any' do
         it 'Delegating script address - any' do
         it 'Enterprise pub key address' do
         it 'Reward account pub key address' do
         it 'Delegating address with both pub key credentials' do
         it 'Delegating address - payment from script, stake from key' do
         it 'Delegating address - payment from key, stake from script' d
       it 'Malformed payload when tx is not binary' do
```


```
shared:      it 'I can create, get and delete wallet from mnemonics getting acc_xpub from cardano-address' do
         it 'I can create, get and delete wallet from pub key getting acc_xpub from cardano-address' do
         it 'Cannot create wallet with different acc xpub - derived from different mnemonic sentence' do
         it 'Cannot create wallet with different acc xpub - derived from different acc ix' do
         it 'I can create incomplete wallet and update cosigners with acc_xpub from cardano-address' do
         it 'Create / update partially / get / list / delete' do
         it 'Cannot update main cosigner' do
           it 'Shared walletid with only spending template from cardano-addresses' do
           it 'Shared walletid with spending and delegation template from cardano-addresses' do
         it 'Can see utxo' do
         it 'Can see utxo snapshot' do
         it 'Can list addresses on active shared wallet - from pub key' do
         it 'Can list addresses on active shared wallet - from mnemonics' do
         it 'Lists empty addresses on incomplete shared wallet - from pub key' do
         it 'Lists empty addresses on incomplete shared wallet - from mnemonics' do
         it 'Get public key - incomplete wallet from mnemonics' do
         it 'Get public key - incomplete wallet from acc pub key' do
         it 'Get public key - active wallet from mnemonics' do
         it 'Get public key - active wallet from acc pub key' do
         it 'Create account public key - incomplete wallet from mnemonics' do
         it 'Cannot create account public key - incomplete wallet from acc pub key' do
         it 'Create account public key - active wallet from mnemonics' do
         it 'Cannot create account public key - active wallet from acc pub key' do
         it 'Get account public key - active wallet from mnemonics' do
         it 'Get account public key - active wallet from acc pub key' do
         it 'Get account public key - incomplete wallet from mnemonics' do
         it 'Get account public key - incomplete wallet from acc pub key' do
       it 'I could get a tx if I had proper id' do
       it 'Can list transactions' do
e2e:    it 'ADP-2523 - Make sure there are no null values in the response', :adp_2523 do
        it 'AlwaysFails.plutus with collateral return to the wallet' do
        it 'cannot balance on empty wallet' do
        it 'ping-pong' do
        it 'game' do
        it 'mint-burn' do
        it 'withdrawal' do
        it 'currency' do
        it 'I can get min_utxo_value when contructing tx' do
        it 'Single output transaction' do
        it 'Multi output transaction' do
        it 'Multi-assets transaction' do
        it 'Only withdrawal' do
        it 'Only metadata' do
        it 'Validity intervals' do
        it 'Delegation (join and quit)' do
          it 'Can mint and then burn' do
          it 'Can mint and burn with metadata' do
          it 'Can mint NFT attaching CIP-25 metadata' do
          it 'Can mint and burn in the same tx' do
          it 'Can mint and burn the same asset in single tx' do
          it 'Cannot burn with wrong policy_script or more than I have' do
          it 'Cannot mint if I make too big transaction' do
          it 'Fixture shelley wallet has utxos' do
          it 'I can list native assets' do
          it 'I can list native assets and get offchain metadata', :offchain do
          it 'I can send native assets tx and they are received' do
          it 'I can create migration plan shelley -> shelley' do
          it 'I can send transaction and funds are received (min_utxo_value)' do
          it 'I can send transaction with ttl and funds are received' do
          it 'Transaction with ttl = 0 would expire and I can forget it' do
          it 'I can send transaction with metadata' do
          it 'I can estimate fee (min_utxo_value)' do
          it 'I can list transactions and limit response with query parameters' do
          it 'I could check delegation fees - if I could cover fee' do
          it 'Can list stake pools only when stake is provided' do
          it 'Can join and quit Stake Pool' do
          it 'I can trigger random coin selection' do
          it 'I can trigger random coin selection delegation action' do
          it 'I could trigger random coin selection delegation action - if I had money' do
          it 'I could trigger random coin selection delegation action - if I known pool id' do
          it 'I can update passphrase with mnemonic and the wallet does not have to re-sync' do
          it 'Fixture shared wallets have utxos' do
          it 'I can estimate fees (min_utxo_value), random' do
          it 'I can estimate fees (min_utxo_value), icarus' do
          it 'I can send transaction and funds are received (min_utxo_value), random -> shelley' do
          it 'I can send transaction and funds are received (min_utxo_value), icarus -> shelley' do
          it 'I can send native assets tx and they are received (random -> shelley)' do
          it 'I can send native assets tx and they are received (icarus -> shelley)' do
          it 'I can list transactions and limit response with query parameters (byron)' do
          it 'I can list transactions and limit response with query parameters (icarus)' do
          it 'I can create migration plan byron -> shelley' do
          it 'I can create migration plan icarus -> shelley' do
          it 'I can list assets -> random' do
          it 'I can list assets -> icarus' do
          it 'I can list assets with offchain metadata -> random', :offchain do
          it 'I can list assets with offchain metadata -> icarus', :offchain do
        it 'Submit tx via POST /proxy/transactions' do
        it 'Cannot submit unsigned tx via POST /proxy/transactions' do
        it 'I can migrate all funds back to fixture wallet' do
byron:    it 'I can list byron wallets' do
          it 'I could get a wallet' do
          it 'I could delete a wallet' do
          it 'I can create, get and delete byron icarus wallet from mnemonics' do
          it 'I can create, get and delete byron random wallet from mnemonics' do
          it 'Can see utxo' do
          it 'Can see utxo snapshot' do
          it 'Can list addresses - random', :adp_2211 do
          it 'Can list addresses - icarus' do
          it 'Can list addresses - ledger' do
          it 'Can list addresses - trezor' do
          it 'Can create address - random' do
          it 'I can import address - random' do
          it 'I cannot import address - icarus' do
          it 'I cannot import address - ledger' do
          it 'I cannot import address - trezor' do
          it 'I could trigger random coin selection - if had money' do
          it 'I could create migration plan - icarus' do
          it 'I could create migration plan - random' do
          it 'I could migrate all my funds' do
shelley:  it 'I can list wallets' do
          it 'When wallet does not exist it gives 404' do
            it 'I can create, get and delete wallet from mnemonics' do
            it 'I can create, get and delete wallet from mnemonics / second factor' do
            it 'I can set address pool gap' do
            it 'I can create, get and delete wallet from pub key' do
              it 'I can get Shelley walletid using cardano-addresses' do
              it 'Shelley walletid is not based on acct key' do
            it 'Can update_metadata' do
            it 'Can update_passphrase' do
            it 'Cannot update_passphrase not knowing old pass' do
            it 'Can update_passphrase, mnemonics' do
            it 'Can update_passphrase, mnemonics, mnemonic_second_factor' do
            it 'Cannot update_passphrase with wrong mnemonics' do
            it 'Cannot update_passphrase with wrong mnemonic_second_factor' do
            it 'Cannot update_passphrase of wallet from pub key' do
            it 'Can update_passphrase of wallet from pub key using mnemonics from which pub key is derived' do
            it 'Cannot update_passphrase of wallet from pub key using wrong mnemonics' do
          it 'Can see utxo' do
          it 'Can see utxo snapshot' do
          it 'Can list addresses' do
          it 'I could trigger random coin selection - if had money' do
          it 'I could get a tx if I had proper id' do
          it 'Can list transactions' do
          it 'I could create transaction - if I had money' do
          it 'I could create transaction using rewards - if I had money' do
          it 'I could estimate transaction fee - if I had money' do
          it 'I could forget transaction' do
          it 'I can list stake keys' do
            it 'pool_metadata_source = direct <> none' do
          it 'I could quit stake pool - if I was delegating' do
          it 'I could create migration plan' do
          it 'I could migrate all my funds' do
          it 'Get signed metadata' do
          it 'Get public key' do
          it 'Create account public key - extended' do
          it 'Create account public key - non_extended' do
          it 'Create account public key - extended with purpose' do
          it 'Create account public key - non_extended with purpose' do
          it 'Get account public key - wallet from acc pub key' do
          it 'Get account public key - wallet from mnemonics' do
          it 'Get account public key (mnemonic_snd_factor)' do
e2e_shared: it 'Fixture shared wallets have utxos' do
            it 'Cannot submit if partially signed - one cosigner, all' do
            it 'Cannot submit if tx is foreign - two cosigners, all' do
            it 'Single output transaction - two cosigners, all' do
            it 'Multi output transaction - two cosigners, all' do
            it 'Multi-assets transaction - two cosigners, all' do
            it 'Validity intervals - two cosigners, all' do
            it 'Only metadata - two cosigners, all' do
            it 'Single output transaction - one cosigner, any' do
            it 'I can get min_utxo_value when contructing tx' do
            it 'Single output transaction' do
            it 'Multi output transaction' do
            it 'Multi-assets transaction' do
            it 'Validity intervals' do
            it 'Only metadata' do
            it 'Delegation (join and quit)' do
                   it 'Can mint and then burn (without submitting)' do
            it 'I can receive transaction to shared wallet' do
            it 'I can list transactions and limit response with query parameters' do
            it 'I can migrate all funds back to fixture shared wallet' do
```
