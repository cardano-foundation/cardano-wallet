# Specification: Encrypting and decrypting metadata

This document specifies those aspects of the "Transactions New > Construct" HTTP endpoint that relate to publishing of encrypted metadata.
In addition "Transactions New > Decode" HTTP endpoint is described in the context of decrypting the metadata.

## Metadata encryption

Encryption of metadata is optional and when chosen the metadata in transaction is to be encrypted
via AES256CBC according to [CIP-0020][cip0020] and [CIP-0083][cip0083].
A PKCS#7 padding of payload is used before encryption as the required
input length must be a multiple of block size, ie., 16 bytes.
PBKDF2 password stretching is used to get a 32-byte symmetric key
that is required for the adopted encryption algorithm. In detail, 
PBKDF2 encryption uses HMAC with the hash algorithm SHA512.

As a consequence the encrypted metadata, not its raw version, is going to be stored in blockchain.

However, in line with [CIP-0020][cip0020] and [CIP-0083][cip0083], only the field `674` of the `metadata` field of the transaction will be affected.

  [cip0020]: https://github.com/cardano-foundation/CIPs/tree/master/CIP-0020
  [cip0083]: https://github.com/cardano-foundation/CIPs/tree/master/CIP-0083

The "Transactions New > Construct" HTTP endpoint allows the encryption of metadata.
The "Transactions New > Decode" HTTP endpoint allows for decrypting of the encrypted metadata.

Specifically:

1. Creation of a transaction output that contains a metadata with encryption enabled.

In the `encrypt_metadata` field, passphrase used in encryption is established. `metadata` field to be encrypted is required.

Example `POST` data for the endpoint, ie.,   /wallets/{walletId}/transactions-construct`:

    ```
    {
    ...
      "encrypt_metadata":
          { "passphrase": "my secret encryption password"
          },
      "metadata":
          { "674" : {
                      "msg": "raw metadata ... "
                    }
          }
    ...
    }
    ```

As a result we get transaction with metadata encrypted:
    ```
    {
    ...
      "metadata":
          { "674":
                   {
                     "enc": "basic",
                     "msg":
                            [
                              "base64-string 1", "base64-string 2", "base64-string 3" ...
                            ]
                   }
          }
    ...
    }
    ```
The same is the case for `GET` transaction. `encrypt_metadata` is an object as we might want to introduce
optional choice of encryption method in the future. In that case the new enhancement to api will be introduced in
non-intrusive way.

Metadata encryption can be used for shared wallet style when calling `/shared-wallets/{walletId}/transactions-construct` endpoint with the same `POST` payload.

Example:
    ```
    {
    ...
      "encrypt_metadata":
          { "passphrase": "metadata-secret"
          },
      "metadata":
          { "674" : {
                      "msg":"world"
                    }
          }
    ...
    }
    ```
will return (for the example salt "yoDCYXKaVhA=")
    ```
    {
    ...
      "metadata":
          { "674" : {
                      "enc": "basic",
                      "msg": [ "U2FsdGVkX1/KgMJhcppWEG6t0aUcMqdEJmnSHVOCgpw=" ]
                    }
          }
    ...
    }
    ```

Example:
    ```
    {
    ...
      "encrypt_metadata":
          { "passphrase": "metadata-secret"
          },
      "metadata":
          { "674" : {
                      "msg":
                          [ "Hard times create strong men."
                          , "Strong men create good times."
                          , "Good times create weak men."
                          , "And, weak men create hard times."
                          ]
                    }
          }
    ...
    }
    ```
will return (for the example salt "XG1cgIw56q8=")
    ```
    {
    ...
          { "674" : {
                      "enc": "basic",
                      "msg":
                          [ "U2FsdGVkX19cbVyAjDnqr5eksQ9gnxJDz6dWhAaXvZGQl31HdEtTpBa91osBavdQ"
                          , "xvOJpGuA8vQGJUgn9RVuqFbVxpggHGCspU6Z5BV5j1LlSqnp6GfHFvrTL3sZcZMq"
                          , "MtOMZSx+d6nPRJL6453wC3rh0cny6SnrEUt9awwxx4PDZk7pDT85h3ygQf1I8fow"
                          , "tYtj3GY0cBwIHfkRLrsxbg=="
                          ]
                    }
          }
    ...
    }
    ```
as metadata values have 64-byte limit. In that case the encrypted metadata is encoded in the successive bytes.


## Metadata decryption

2. Decoding transaction with encrypted metadata is possible by using the same passphrase as upon encryption in `encrypt_metadata` field. It is realized by calling `POST` on `/wallets/{walletId}/transactions-decode` endpoint with `POST` data:

    ```
    {
      "decrypt_metadata":
          { "passphrase": "my secret encryption password"
          },
      "transaction": ....
    }
    ```

As a result we get decoded transaction with metadata decrypted:
    ```
    {
    ...
      "metadata":
          { "674" : {
                      "msg": "raw metadata ... "
                    }
          }
    ...
    }
    ```

Metadata decryption can be used for shared wallet style when calling `/shared-wallets/{walletId}/transactions-decode` endpoint with the same `POST` payload.
