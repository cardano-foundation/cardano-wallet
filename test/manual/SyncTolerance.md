# Tests for `--sync-tolerance` on cardano-wallet

## OS

Windows, MacOS, Linux

1. Launch wallet and jormungandr in separate terminals

```bash
$ jormungandr \
     --genesis-block lib/jormungandr/test/data/jormungandr/block0.bin \
     --rest-listen 127.0.0.1:8080 \
     --secret lib/jormungandr/test/data/jormungandr/secret.yaml

$ cardano-wallet-jormungandr serve \
     --genesis-block-hash $(jcli genesis hash --input lib/jormungandr/test/data/jormungandr/block0.bin) \
     --node-port 8080
```

2. Poll network information until the `sync_progress` is marked as `"ready"`

```bash
$ cardano-wallet-jormungandr network information | jq .sync_progress.status
Ok.
"ready"
```


3. Stop the wallet server for at least 30 seconds, but **keep Jörmungandr running**.


4. Restart the server with a `--sync-tolerance` **short** in front of 30 seconds

```bash
$ cardano-wallet-jormungandr serve \
     --genesis-block-hash $(jcli genesis hash --input lib/jormungandr/test/data/jormungandr/block0.bin) \
     --node-port 8080 \
     --sync-tolerance 1s
```


5. Quickly query the network information and check that the `sync_progress` is `"syncing"`.

```bash
$ cardano-wallet-jormungandr network information | jq .sync_progress.status
Ok.
"syncing"
```


6. Stop the wallet server for at least 30 seconds, but **keep Jörmungandr running**.


7. Restart the server with a `--sync-tolerance` **large** in front of 30 seconds

```bash
$ cardano-wallet-jormungandr serve \
     --genesis-block-hash $(jcli genesis hash --input lib/jormungandr/test/data/jormungandr/block0.bin) \
     --node-port 8080
     --sync-tolerance 90s
```


8. Quickly query the network information and check that the `sync_progress` is `"ready"`.

```bash
$ cardano-wallet-jormungandr network information | jq .sync_progress.status
Ok.
"ready"
```
