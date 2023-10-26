# Specification: Encrypting and decrypting metadata

This document specifies those aspects of the "Transactions New > Construct" HTTP endpoint that relate to publishing of encrypted metadata.
In addition "Transactions New > Decode" HTTP endpoint is described in the context of decrypting the metadata.

## Metadata encryption

Encryption of metadata is optional and when chosen the metadata in transaction is to be encrypted
via AEAD scheme using ChaCha20 and Poly1305 (see [RFC 7539][ref]). PBKDF2 password stretching is used to get a 32-byte symmetric key
that is required for the adopted encryption algorithm. In detail, PBKDF2 encryption uses HMAC with the hash algorithm SHA512.
As a consequence the encrypted metadata, rather than raw, is going to be stored in blockchain.

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
