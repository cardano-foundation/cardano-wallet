# Signing via external tools.

Here are instructions how to derive respective keys via cardano-addresses, then sign
transactions CBOR and send the updated CBOR using cardano-cli

Assumptions:
User have either mnemonic, *phrase.prv*, or extended account private key, *acct.xsk*.
User know which credential payment index is going to be used, *ix*.
Unsigned or partially signed CBOR of tx is available.

Goal is to construct CBOR that is input CBOR with properly added witness from the credential signing key
being a dual of the input credential payment verificiation key.

1. In case of missing *acct.xsk* it can be derived as follows:
```bash
$ cat  phrase.prv
attract flight range human visual record trade mango chunk enough satoshi galaxy grit van shrug

$ cardano-address key from-recovery-phrase Shelley < phrase.prv > root.xsk
$ root.xsk
$ cat root.xsk
root_xsk1dqh2lewgwnfzf0kreek8c2zx9csq2d8nh9ku5tvkkxjzypuy5402qnxrl3htj84qxchuxueg3nt7uv50v2mj9vynpdckslyvc24qqxeysye4h2c0cgdemujn8mcprgcstgjvkep30ygu4p3ch983chukqvusp4yk

$ cardano-address key child 1857H/1815H/0H < root.xsk > acct.xsk
$ cat acct.xsk
acct_xsk10zeunvvghchkcg8w6achyn3usv642alx8f57rv9kzdzt7yuy540xs4r08lwq576a3v822z9jv8v7kjkjpqxdqtjzt4ukh6w8f57vg0fv6dzaq33pps7hwe5f70cztch0z7kj0552felguwn6n2u74h248g5na6u5
```

2. Deriving extended verification keys and signing keys for role=0 and address ix=0, and hash that is credential
```bash
$ cardano-address key child 0/0 < acct.xsk > key.xsk
$ cat key.xsk
addr_xsk14pwgnh4q757kgfjn2w83prmh27kj58ety3acvt0jvx2lwxvy540gmd5gug2egr9dlzv4z04nm9jd26al494w9t6qhlzf07re2myu9mv7syp6aym49c0d97lfg8y0c36vgjv54qnwte6rz3f6x0ltnjqfwcnufn8e

$ cardano-address key public --with-chain-code < key.xsk > key.xvk
$ cat key.xvk
addr_xvk1dkqjgyp2tdq0a0tre7qlhprdr88r497k072q0726lhux4xyfxtcfaqgr46fh2ts76ta7jswgl3r5c3yef2pxuhn5x9zn5vl7h8yqjas27h22j

$ cardano-address key hash < key.xvk
addr_vkh1k70phz25qm9g6uxxguw8znnepqc5uu2mqx9yd7ea8yc7urscytf
$ cardano-address key hash --hex < key.xvk
b79e1b895406ca8d70c6471c714e7908314e715b018a46fb3d3931ee
```

3. Constructing enterprise address for preprod using *key.xvk*
```bash
$ cardano-address address payment --network-tag testnet < key.xvk
addr_test1vzmeuxuf2srv4rtscer3cu2w0yyrznn3tvqc53hm85unrmsg4m9cg
```

4. Mapping *key.xsk* to the key suitable for cardano-cli
```bash
$ cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file key.xsk --out-file key.skey
$ cat key.skey
{
    "type": "PaymentExtendedSigningKeyShelley_ed25519_bip32",
    "description": "",
    "cborHex": "5880a85c89dea0f53d642653538f108f7757ad2a1f2b247b862df26195f71984a55e8db688e215940cadf899513eb3d964d56bbfa96ae2af40bfc497f87956c9c2ed6d8124102a5b40febd63cf81fb846d19ce3a97d67f9407f95afdf86a988932f09e8103ae93752e1ed2fbe941c8fc474c44994a826e5e7431453a33feb9c80976"
}
```
Remark: *cborHex* contains:
- prefix 5880 staking that the bytestring is 128 bytes
- signing key (64 bytes)
- verification key (32 bytes)
- chain code (32 bytes)
One can confirm this using `cardano-address key inspect`, `cardano-address key public` and `cardano-address key private` options

5. The corresponding verification key
```bash
$ cardano-cli key verification-key --signing-key-file key.skey --verification-key-file key.vkey
$ cat key.vkey
{
    "type": "PaymentExtendedVerificationKeyShelley_ed25519_bip32",
    "description": "",
    "cborHex": "58406d8124102a5b40febd63cf81fb846d19ce3a97d67f9407f95afdf86a988932f09e8103ae93752e1ed2fbe941c8fc474c44994a826e5e7431453a33feb9c80976"
}
```
Remark: *cborHex* contains:
- prefix 5840 staking that the bytestring is 64 bytes
- verification key (32 bytes)
- chain code (32 bytes)

6. The corresponding key hash (the same like in point 2 above)
```bash
$ cardano-cli address key-hash --payment-verification-key-file key.vkey
b79e1b895406ca8d70c6471c714e7908314e715b018a46fb3d3931ee
```

7. Signing using cardano-cli. Here let's assume we have unsigned tx and we will use the above keys.
```bash
$ cat tx.unsigned
{
    "type":"Unwitnessed Tx ConwayEra",
    "description":"Ledger Cddl Format",
    "cborHex":"84a400d90102818258204fe1968fc521dffe2bb9799b9c6548e38cd5e1a593c7d43a251eeb92deadc3fe00018282581d60d23d12a37c21b84c8c7838d4bbda848fe7a6b7bfc3f54212238912ec1a000f424082581d601cbb2cdd51437bb9f43bdd1214984e8b2794e0cff25f47ba187494041b0000000253fa1907021a000288b9031a0498a97aa0f5f6"}

$ cardano-cli conway transaction sign --signing-key-file key.skey --testnet-magic 1 --tx-body-file tx.unsigned --out-file tx.signed
$ cat tx.signed
{
    "type": "Witnessed Tx ConwayEra",
    "description": "Ledger Cddl Format",
    "cborHex": "84a400d90102818258204fe1968fc521dffe2bb9799b9c6548e38cd5e1a593c7d43a251eeb92deadc3fe00018282581d60d23d12a37c21b84c8c7838d4bbda848fe7a6b7bfc3f54212238912ec1a000f424082581d601cbb2cdd51437bb9f43bdd1214984e8b2794e0cff25f47ba187494041b0000000253fa1907021a000288b9031a0498a97aa100d90102818258206d8124102a5b40febd63cf81fb846d19ce3a97d67f9407f95afdf86a988932f058401a4757dc289f97684339ec766d1fcddfe1ebd50a53d7cccbb71b265e784dd6eb4bf87d5b6c2383e66f1a679f2ac0d97add6a890779096f0802690518223a8c04f5f6"
}
```

8. Submitting the signed tx
```bash
$ cardano-cli conway transaction submit --tx-file  tx.signed --testnet-magic 1
```
