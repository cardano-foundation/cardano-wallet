# Specification: Encrypting and decrypting metadata

This document specifies those aspects of the "Transactions New > Construct" HTTP endpoint that relate to publishing of encrypted metadata.
In addition "Transactions New > Decode" HTTP endpoint is described in the context of decrypting the metadata.

## Metadata encryption

Encryption of metadata is optional and when chosen the metadata in transaction is to be encrypted
via AEAD scheme using ChaCha20 and Poly1305 (see [RFC 7539][ref]). PBKDF2 password stretching is used to get a 32-byte symmetric key
that is required for the adopted encryption algorithm. In detail, PBKDF2 encryption uses HMAC with the hash algorithm SHA512.
As a consequence the encrypted metadata, not its raw version, is going to be stored in blockchain.

  [ref]: https://datatracker.ietf.org/doc/html/rfc7539

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
      "metadata": "raw metadata"
    ...
    }
    ```

    As a result we get transaction with metadata encrypted:
    ```
    {
    ...
      "metadata": "metadata encrypted"
    ...
    }
    ```
    The same is the case for `GET` transaction. `encrypt_metadata` is an object as we might want to introduce
    optional choice of encryption method in the future. In that case the new enhancement to api will be introduced in
    nonintrusive way.

    Metadata encryption can be used for shared wallet style when calling `/shared-wallets/{walletId}/transactions-construct` endpoint with the same `POST` payload.

    Example:
    ```
    {
    ...
      "encrypt_metadata":
          { "passphrase": "metadata-secret"
          },
      "metadata": {"1":"hello"}
    ...
    }
    ```
    will return
    ```
    {
    ...
      "metadata": {"0":"0x0aa4f9a016215f71ef007b60601708dec0d10b4ade6071b387295f95b4"}
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
          { "1": "Hard times create strong men."
          , "2": "Strong men create good times."
          , "3": "Good times create weak men."
          , "4": "And, weak men create hard times."
          }
    ...
    }
    ```
    will return
    ```
    {
    ...
      "metadata":
         { "0": "0x0aa4f9a016217f75f10834367493f6d7e74197417ca25c7615cae02bc345382906fb6990daf8f138b2d9192e057d0d0b555f9d5fb287abb1842928c90f26e597"
         , "1": "0x559ee85f00f1588b3ee32e81dc4c84aee208a10c1eec97fffe6e0e66c69d4e0b1e3e22d7edc1618df3b20b484527d86bc3bebad4295a2ad888d034b5fec38077"
         , "2": "0x8d42154f681230124c64630ea68b841aec22f0530ec830cb662d59ef423ef23d7ff3"
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
      "metadata": "raw metadata"
    ...
    }
    ```

    Metadata decryption can be used for shared wallet style when calling `/shared-wallets/{walletId}/transactions-decode` endpoint with the same `POST` payload.
