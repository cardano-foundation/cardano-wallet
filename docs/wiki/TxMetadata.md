Since the Shelley era, the Cardano blockchain allows user-defined data to be associated with transactions.

The metadata hash is part of the transaction body, so is covered by all transaction signatures.

The cardano-wallet API server uses a JSON representation of transaction metadata, isomorphic to the binary encoding on chain.

The full JSON schema is specified in the [cardano-wallet OpenAPI Specification](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postTransaction) / [`swagger.yaml`](https://github.com/input-output-hk/cardano-wallet/search?l=YAML&q=TransactionMetadataValue).

### Structure

The top level is a JSON object mapping from metadata keys to metadata values.

**Metadata keys** are integers in the range 0 to 2<sup>64</sup> - 1.

**Metadata values** are one of three simple types or two compound types.

Every metadata value is tagged with its type, using a JSON object.

Simple types:

 * _Integers_ in the range -(2<sup>64</sup> - 1) to 2<sup>64</sup> - 1

    `{ "int": NUMBER }`

 * _Strings_ (UTF-8 encoded)

    `{ "string": STRING }`

 * _Bytestrings_

    `{ "bytes": HEX-STRING }`

    The value must be base16-encoded (a hex string).

Compound types:

 * Lists of _metadata values_

   `{ "list": [ METADATA-VALUE, ... ] }`

 * Mappings from _metadata values_ to _metadata values_

   `{ "map": [{ "k": METADATA-VALUE, "v": METADATA-VALUE }, ... ] }`

Note that lists and maps need not necessarily contain the same type of metadata value in each element.

### Limits

 - Strings may be at most 64 bytes long when UTF-8 encoded.
 - Unencoded bytestrings may be at most 64 bytes long.
 - There are no limits to the number of metadata values, apart from the protocol limit on transaction size.

### Examples

This is a transaction metadata which contains four values.

```json
{
  "64": {"string": "some text"},
  "32": {"int": 42},
  "16": {
    "map": [
      {
        "k": {"string": "numbers"},
        "v": {"list": [{"int": 1}, {"int": 2}, {"int": 4}, {"int": 8}]}
      },
      {
        "k": {"string": "alphabet"},
        "v": {
          "map": [
            {"k": {"string": "A"}, "v": {"int": 65}},
            {"k": {"string": "B"}, "v": {"int": 66}},
            {"k": {"string": "C"}, "v": {"int": 67}}
          ]
        }
      }
    ]
  },
  "8": { "bytes": "48656c6c6f2c2043617264616e6f21" }
}
```

### Sample code: Converting from JavaScript objects

Use a function like this to translate arbitrary JavaScript values into metadata JSON format. If your application requires a more precise mapping, it can be modified to suit. Note that this code does not validate strings for length.

```javascript
#!/usr/bin/env node

function txMetadataValueFromJS(jsVal) {
  if (Array.isArray(jsVal)) {
    // compound type - List
    // (note that sparse arrays are not representable in JSON)
    return { list: jsVal.map(txMetadataValueFromJS) };
  } else if (typeof(jsVal) === 'object') {
    if (jsVal !== null ) {
      // compound type - Map with String keys
      return { map: Object.keys(jsVal).map(key => {
        return { k: { string: key }, v: txMetadataValueFromJS(jsVal[key]) };
      }) };
    } else {
      // null: convert to simple type - String
      return { string: 'null' };
    }
  } else if (Number.isInteger(jsVal)) {
    // simple type - Int
    return { int: jsVal };
  } else if (typeof(jsVal) === 'string') {
    // simple type - String
    return { string: jsVal };
  } else {
    // anything else: convert to simple type - String.
    // e.g. undefined, true, false, NaN, Infinity.
    // Some of these can't be represented in JSON anyway.
    // Floating point numbers: note there can be loss of precision when
    // representing floats as decimal numbers
    return { string: '' + jsVal };
  }
}

// Get JSON objects from stdin, one per line.
const jsVals = require('fs')
  .readFileSync(0, { encoding: 'utf8' })
  .toString()
  .split(/\r?\n/)
  .filter(line => !!line)
  .map(JSON.parse);

// Convert to transaction metadata JSON form
const txMetadataValues = jsVals.map(txMetadataValueFromJS);
const txMetadata = txMetadataValues
  .reduce((ob, val, i) => { ob['' + i] = val; return ob; }, {});

// Print JSON to stdout
console.log(JSON.stringify(txMetadata));
```


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


### References

For a detailed explanation of the metadata design, and information about the transaction format, consult the following specifications.

1. [Delegation Design Spec][delegation-spec], Appendix E.
2. [Cardano Shelley Ledger Spec][shelley-ledger-spec], Figure 10.

[delegation-spec]: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec
[shelley-ledger-spec]: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.shelley-ledger/latest/download-by-type/doc-pdf/ledger-spec
