# Handle Metadata

## Pre-requisites

- [how to start a server](start-wallet-server.md)
- [how to create a wallet](create-a-wallet.md)
- In order to be able to send transactions, our wallet must have funds. In case of `preview` and `preprod` [testnets](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

## Transaction Metadata

Since the Shelley era, the Cardano blockchain allows user-defined data to be associated with transactions.

The metadata hash is part of the transaction body, so is covered by all transaction signatures.

The cardano-wallet API server uses a JSON representation of transaction metadata, isomorphic to the binary encoding on chain.

```admonish warning
Please note that metadata provided in a transaction will be stored on the blockchain forever. Make sure not to include any sensitive data, in particular personally identifiable information (PII).
```

## Metadata structure on chain

The **top level** is a map from metadata keys to metadata values.

**Metadata keys** are integers in the range 0 to 2<sup>64</sup> - 1.

**Metadata values** are one of three simple types or two compound types.

Simple types:

 * _Integers_ in the range -(2<sup>64</sup> - 1) to 2<sup>64</sup> - 1

 * _Strings_ (UTF-8 encoded)

 * _Bytestrings_

Compound types:

 * Lists of _metadata values_

 * Mappings from _metadata values_ to _metadata values_

Note that lists and maps need not necessarily contain the same type of metadata value in each element.

### Limits

 - Strings may be at most 64 bytes long when UTF-8 encoded.
 - Unencoded bytestrings may be at most 64 bytes long (i.e. at most 128 hex digits).
 - There are no limits to the number of metadata values, apart from the protocol limit on transaction size.

The string length limitation is explained in the
[Delegation Design Spec, section E.3][delegation-spec]:

> The size of strings in the structured value is limited to mitigate
> the problem of unpleasant or illegal content being posted to the
> blockchain. It does not prevent this problem entirely, but it means
> that it is not as simple as posting large binary blobs.

## JSON representation in cardano-wallet

The **top level** is a JSON object mapping **metadata keys** as
decimal number strings to JSON objects for metadata values.

Every **metadata value** is tagged with its type, using a JSON object,
like this:

 * _Integers_

    `{ "int": NUMBER }`

 * _Strings_

    `{ "string": STRING }`

 * _Bytestrings_

    `{ "bytes": HEX-STRING }`

    The value must be base16-encoded (a hex string).

 * Lists of _metadata values_

   `{ "list": [ METADATA-VALUE, ... ] }`

 * Mappings from _metadata values_ to _metadata values_

   `{ "map": [{ "k": METADATA-VALUE, "v": METADATA-VALUE }, ... ] }`

## Examples

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

## Sample code: Converting from JavaScript objects

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


## CLI

Metadata can be provided when creating transactions through the
[cli](../cli.md).

The JSON is provided directly as a command-line argument.
 - On Linux/MacOS JSON metadata can be put inside single quotes:
```
--metadata '{ "0":{ "string":"cardano" } }'
```
 - On Windows it can be put in double quotes with double quotes inside JSON metadata escaped:
```
--metadata "{ \"0\":{ \"string\":\"cardano\" } }"
```


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


## References

For a detailed explanation of the metadata design, and information about the transaction format, consult the following specifications.

1. [Delegation Design Spec][delegation-spec], Appendix E.
2. [Cardano Shelley Ledger Spec][shelley-ledger-spec], Figure 10.


The full JSON schema is specified in the OpenAPI 3.0 `swagger.yaml`.

1. [cardano-wallet OpenAPI Documentation][swagger-doc], Shelley / Transactions / Create.
2. [cardano-wallet OpenAPI Specification][swagger-spec], scroll to `TransactionMetadataValue`.


[delegation-spec]: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec
[shelley-ledger-spec]: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.shelley-ledger/latest/download-by-type/doc-pdf/ledger-spec
[swagger-doc]: https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/postTransaction
[swagger-spec]: https://github.com/cardano-foundation/cardano-wallet/blob/master/specifications/api/swagger.yaml
