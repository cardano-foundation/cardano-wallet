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
