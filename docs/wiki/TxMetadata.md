Since the Shelley era, the Cardano blockchain allows user-defined data to be associated with transactions.

The metadata hash is part of the transaction body, so is covered by all transaction signatures.

The cardano-wallet API server uses a JSON representation of transaction metadata, isomorphic to the binary encoding on chain.

The full JSON schema is specified in the [cardano-wallet OpenAPI Specification](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postTransaction).

### Structure

The top level is a map from metadata keys to metadata values.

**Metadata keys** are integers in the range 0 to 2<sup>64</sup> - 1.

**Metadata values** are one of three simple types or two compound types.

Simple types:

 * Integers in the range -(2<sup>64</sup> - 1) to 2<sup>64</sup - 1
 * Strings (UTF-8 encoded)
 * Bytestrings

Compound types:

 * Lists of metadata values
 * Mappings from metadata values to metadata values
 
Note that lists and maps need not necessarily contain the type of metadata value in each element.

### Limits

 - Strings may be at most 64 bytes long when UTF-8 encoded.
 - Unencoded bytestrings may be at most 64 bytes long.
 - There are no limits to the number of metadata values, apart from the protocol limits for transaction size.

### Examples


### Converting from JSON


### CLI

Metadata can be provided when creating transactions through the [Wallet CLI](./Wallet-command-line-interface).

The JSON is provided directly as a command-line argument. Ensure it is single-quoted so that the shell does not try to interpret the JSON syntax. If using the bash shell, you can insert the contents of a file on the command-line using `--metadata $(<tx-meta.json)`.

```
Usage: cardano-wallet transaction create [--port INT] WALLET_ID
                                         --payment PAYMENT [--metadata JSON]
  Create and submit a new transaction.

Available options:
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
  --payment PAYMENT        address to send to and amount to send separated by @,
                           e.g. '<amount>@<address>'
  --metadata JSON          Application-specific transaction metadata as a JSON
                           object. The value must match the schema defined in
                           the cardano-wallet OpenAPI specification.
```


> `cardano-wallet transaction create [--port=INT] WALLET_ID [--metadata=JSON] --payment=PAYMENT...`

### References

For a detailed explanation of the metadata design, and information about the transaction format, consult the following specifications.

1. [Delegation Design Spec][delegation-spec], Appendix E.
2. [Cardano Shelley Ledger Spec][shelley-ledger-spec], Figure 10.

[delegation-spec]: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec
[shelley-ledger-spec]: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.shelley-ledger/latest/download-by-type/doc-pdf/ledger-spec
