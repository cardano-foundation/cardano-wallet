# Specification: One change address mode

This document specifies the "one change address mode",
which indicates that the wallet should use a single, fixed
address for change outputs.
This mode reduces the resource consumption of the wallet
at the expense of privacy.

This mode is available for Shelley-style and Shared-style wallets.

## Description

When balancing a transaction, the wallet selects
unspent transaction outputs (UTxO) to
cover the intended payments and the transaction fee.
Any excess funds will be sent back to a *change address*
which belongs to the wallet.

By default, the Shelley-, Shared-, and Byron-style wallets
create a new change address for every new transaction
in order to enhance privacy. However, when the "one change address mode" is enabled, a single, fixed address will be used for every change output.
This reduces the number of addresses that the wallet creates,
but makes it easier for outsiders to track which funds this wallet owns.

The "one change address mode" can be toggled at any time.
Change addresses that have been created in the past will stay known to the wallet, and the wallet will continue to discover funds at those addresses. The mode only reduces the creation of new addresses.

The creation of new addresses enhances privacy: For an outsider, it is hard to tell whether the funds at a new change address belong to the wallet or not by looking at the payment part of the address alone. In contrast, a wallet is easy to identify if it always uses the same payment part for its change outputs. That said, do note that additional information, such as the staking part of the address or the size of the change output, can be used to identify the wallet.

## One change address mode in HTTP API

The "Wallets > Post" HTTP endpoint allows the creation of the wallet that is in *one change address mode*.

Specifically:

1. Creation of a shelley/shared wallet

    When the `one_change_address_mode` field is set to `true`, the created wallet will reuse the first change address when
    transactions are made. When the wallet is restored all used change addresses will be discovered but new transactions will
    reuse the first change address.

    Example `POST` data for the endpoint:

    ```
    {
    ...
      "one_change_address_mode": true,
    ...
    }
    ```

2. Updating metadata of wallet

    It is possible to toggle the one change address mode.
    Turning the mode on will stop the creation of new change addresses, turning it off will resume such creation.

    Example `PUT` data for the endpoint to turn off one change address mode:

    ```
    {
      "one_change_address_mode": false
    }
    ```
