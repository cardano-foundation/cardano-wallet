# Feature Specification

## Reference inputs for minting/burning in shelley style

Reference input capability enables defining a script in one transaction and then
use it via reference in the subsequent transaction(s). Blockchains acts as
an intermediary allowing temporal and spacial separation between the definition
of a script and its usage.

Construct transaction HTTP endpoint accommodates both stages

1. There is an optional field `reference_policy_script_template:`
   that is the script template which is mapped into a script
   with the wallet's policy public key. The script is included in
   a first transaction output (ie. at index 0). This is the script defining
   stage.

2. In order to mint/burn an asset the `mint_burn` field needs to be specified.
   `policy_script_template` is used when one specifies the minting/burning script.
   `reference_input` is used when a script is already defined in one of the outputs.
   User needs to specifty transaction id of the transaction were script was defined and
   output index (which is 0 per design of stage 1).
