
# Error reported in API when failing to fetch metadata

## OS

Any

## Steps

1. Start cardano-node and cardano-wallet on `testnet` using latest [config](https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/index.html). Make sure both are fully synced.

```bash
$ cardano-node run \
		--config ./*-config.json \
		--topology ./*-topology.json \
		--database-path ./db \
		--socket-path ./node.socket

$ cardano-wallet serve --port 8090 \
		--node-socket ../relay1/node.socket \
		--testnet testnet-byron-genesis.json  \
		--database ./wallet-db \
		--token-metadata-server http://localhost/
```
Make sure wallet server `--token-metadata-server` points to an url which is not token metadata server.

2. On the wallet that has some assets check that `"metadata_error":"fetch"` is returned when getting assets.
 - when listing all assets
```
$ curl -X GET http://localhost:8090/v2/wallets/73d38c71e4b8b5d71769622ab4f5bfdedbb7c39d/assets

[
...
{
	"asset_name": "496e737472756d656e74616c4f4b3133",
	"fingerprint": "asset123svc349ccm6njzpu6w3uzu6cnw87t9l95dmcy",
	"metadata_error": "fetch",
	"policy_id": "c7b3f8f9d6a02027e7b5ce2a4da0ff3f68d6d49d4ca58c96c5900b37"
},
...
]
```
- when getting asset with only policy id
```
$ curl -X GET http://localhost:8090/v2/wallets/73d38c71e4b8b5d71769622ab4f5bfdedbb7c39d/assets/789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1

{
  "asset_name": "",
  "fingerprint": "asset1656gm7zkherdvxkn52mhaxkkw343qtkqgv0h8c",
  "metadata_error": "fetch",
  "policy_id": "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1"
}
```
 - when getting asset with policy id and asset name
```
$ $ curl -X GET http://localhost:8090/v2/wallets/73d38c71e4b8b5d71769622ab4f5bfdedbb7c39d/assets/005bd7d46219700eccb77cbf7122055a0b26cd064db51e1277cc1b0b/653265636f696e3635

{
  "asset_name": "653265636f696e3635",
  "fingerprint": "asset1txvs0057gd63g02q4ttrd86vgxlv3scjua7r03",
  "metadata_error": "fetch",
  "policy_id": "005bd7d46219700eccb77cbf7122055a0b26cd064db51e1277cc1b0b"
}

```
