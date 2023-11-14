# Specification: DRep voting

This document specifies those aspects of the "Transactions New > Construct" HTTP endpoint that relate to DRep voting in Voltaire era.

## Voting during delegation and delegate with abstain

The Voltaire voting system interacts with stake pool delegation. The following options are available:
1. Delegate to a stake pool without participating in the voting system.
2. Delegate to a stake pool while also participating in the voting system.

After a grace period (6–12 weeks), it will not be possible to withdraw staking rewards under option 1. (The rewards will still be accumulated, and are redeemable again once the user switches to option 2.)
In order to allow passive participation in the voting system, as part of option 2, there exists the possibility to delegate to a decentralized representative (DRep) called `Abstain`, who will always vote to abstain from any particular decision. While very passive,  the spirit of voting system participation still requires that the user actively opts into this voting preference.

The "Transactions New > Construct" HTTP endpoint allows delegation and withdrawals.

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
