# Specification: Minting and Burning

This document specifies those aspects of the "Transactions New > Construct" HTTP endpoint that relate to minting and burning native assets.

## Reference inputs for minting scripts

[Reference inputs][ref] are inputs of a transaction that are not spent; instead, the outputs they reference are made available to, say, Plutus validator scripts for inspection.

  [ref]: https://cips.cardano.org/cips/cip31/#referenceinputs

In particular, with reference inputs, we can define a minting script in one transaction and reference the script in subsequent transaction(s). In other words, we can save space on the blockchain by defining a script once and referencing it later, instead of including a full copy of the script with every transaction.

The "Transactions New > Construct" HTTP endpoint allows the creation and use of reference inputs.

Specifically:

1. Creation of a transaction output that contains a minting script and is suitable for use as reference input.

    In the `reference_policy_script_template` field, you can optionally add a script template. The HTTP endpoint will map this script template into a script using the wallet's policy public key, and this script will be included in the first transaction output (i.e. at index `0`) of the transaction.

    Example `POST` data for the endpoint:

    ```
    {
    ...
      "validity_interval": ...
      "reference_policy_script_template": { all: ['cosigner#0', { active_from: 120 }] },
      "encoding": ...
    ...
    }
    ```

2. Using a refence input that contains a minting script.

    In the `mint_burn` field, the array element `policy_script_template` contains an option `reference_input`. This option specifies a transaction input (pair of transaction ID and output index) which will be added as a reference input and is assumed to contain the minting script. (If the corresponding output was created using the method above, the appropriate output index is `0`).

    Example `POST` data for the endpoint:

    ```
    { "mint_burn": [???]
    }
    ```
