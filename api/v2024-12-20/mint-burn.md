# Specification: Minting and Burning

This document specifies those aspects of the "Transactions New > Construct" HTTP endpoint that relate to minting and burning native assets.

## Reference inputs for minting scripts

[Reference inputs][ref] are inputs of a transaction that are not spent; instead, the outputs they reference are made available to, say, Plutus validator scripts for inspection.

  [ref]: https://cips.cardano.org/cips/cip31/#referenceinputs

In particular, with reference inputs, we can define a minting script in one transaction and reference the script in subsequent transaction(s). In other words, we can save space on the blockchain by defining a script once and referencing it later, instead of including a full copy of the script with every transaction.

The "Transactions New > Construct" HTTP endpoint allows the creation and use of reference inputs.

Specifically:

1. Creation of a transaction output that contains a minting script and is suitable for use as reference input.

    In the `reference_policy_script_template` field, you can optionally add a script template. The HTTP endpoint will map this script template into a script using the wallet's policy public key, and this script will be included in the first transaction output (i.e. at index `0`) of the transaction. For Shelley-style wallets, the script template must contain a single cosigner only, but it may include time locks.

    Example `POST` data for the endpoint:

    ```
    {
    ...
      "reference_policy_script_template":
          { "all":
             [ "cosigner#0",
               { "active_from": 120 }
             ]
          },
    ...
    }
    ```

2. Getting policy id using the same script template as in point 1. It is realized by calling `POST` on `/wallets/{walletId}/policy-id` endpoint with `POST` data:

    ```
    {
      "policy_script_template":
          { "all":
             [ "cosigner#0",
               { "active_from": 120 }
             ]
          }
    }
    ```

3. Using a refence input that contains a minting script.

    In the `mint_burn` field, the array element contains `reference_input` and `policy_id`. The first field specifies a transaction input (pair of transaction ID and output index) which will be added as a reference input and is assumed to contain the minting script. (If the corresponding output was created using the method above, the appropriate output index is `0`). The `policy_id` field is obtained from the response of request realized in point 2.

    Example `POST` data for the endpoint with reference input:

    ```
    {
    ...
    "mint_burn": [{
        "reference_input":
            { "id": "464917d2bac71df96269c2d7c34dcb83183b8a3a3253c06e9d6a8bd0681422c9",
              "index": 0
            },
        "policy_id": "7191ae0e1286891fe5c027a5dc041b7401689938e18e14ec83cf74fb",
        "asset_name": "ab12",
        "operation":
            { "mint" :
                  { "receiving_address": #{destination},
                     "quantity": 10000
                  }
            }
    }]
    ...
    }
    ```

    For contrast: Example `POST` data for the endpoint with script template:

    ```
    {
    ...
    "mint_burn": [{
        "policy_script_template":
            { "all":
               [ "cosigner#0",
                 { "active_from": 120 }
               ]
            },
        "asset_name": "ab12",
        "operation":
            { "mint" :
                  { "receiving_address": #{destination},
                     "quantity": 10000
                  }
            }
    }]
    ...
    }
    ```
