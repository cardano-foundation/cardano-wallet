Since the Shelley era, the Cardano blockchain allows user-defined data to be associated with transactions.

The full schema is documented in the [cardano-wallet OpenAPI Specification](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postTransaction).

### Examples

TODO

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

See Appendix E of the [Delegation Design Spec][delegation-spec] and Figure 10 of the [Cardano Shelley Ledger Spec][shelley-ledger-spec] for detailed information about the transaction format.

[delegation-spec]: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec
[shelley-ledger-spec]: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.shelley-ledger/latest/download-by-type/doc-pdf/ledger-spec
