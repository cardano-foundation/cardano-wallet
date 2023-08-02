# DB Migration from previously released version

## OS

Windows, MacOS, Linux

**strongly recommended to test on all platforms**

## Start previous version of the wallet

1. Get **previous** release -> https://github.com/cardano-foundation/cardano-wallet/releases
2. Start cardano-node and cardano-wallet on `testnet` using latest [config](https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest/download/1/index.html). Make sure both are fully synced.

```bash
$ cardano-node run \
		--config ./*-config.json \
		--topology ./*-topology.json \
		--database-path ./db \
		--socket-path ./node.socket

$ cardano-wallet serve --port 8090 \
		--node-socket ../relay1/node.socket \
		--testnet testnet-byron-genesis.json  \
		--database ./wallet-db
```

3. Produce some data in the cardano-wallet DB e.g.
 - add shelley wallet
 ```bash
$ curl -vX POST http://localhost:8090/v2/wallets \
	-H "Content-Type: application/json; charset=utf-8" \
	-d '{
			"name": "Shelley",
			"mnemonic_sentence": ["identify", "screen", "lock", "bargain", "inch", "drop", "canyon", "flock", "dry", "zone", "wash", "argue", "system", "glory", "light"],
			"passphrase": "Secure Passphrase",
			"address_pool_gap": 20
			}'
 ```

 - add byron wallets
 ```bash
$ curl -vX POST http://localhost:8090/v2/byron-wallets \
	-H "Content-Type: application/json; charset=utf-8" \
	-d '{
			"style" : "random",
			"name": "Random",
			"mnemonic_sentence": ["rotate", "machine", "travel", "safe", "expire", "leopard", "wink", "vault", "borrow", "digital", "wisdom", "harsh"],
			"passphrase": "Secure Passphrase"
			}'

$ curl -vX POST http://localhost:8090/v2/byron-wallets \
	-H "Content-Type: application/json; charset=utf-8" \
	-d '{
			"style" : "icarus",
			"name": "Icarus",
			"mnemonic_sentence": ["rotate", "machine", "travel", "safe", "expire", "leopard", "wink", "vault", "borrow", "digital", "wisdom", "harsh"],
			"passphrase": "Secure Passphrase"
			}'
 ```
 - send tx
 ```bash
$ curl -vX POST http://localhost:8090/v2/wallets/617963656a409b8a6828dc3a09001de22af90400/transactions \
	-H "Accept: application/json; charset=utf-8" \
	-H "Content-Type: application/json; charset=utf-8" \
	-d '{
	"payments": [
	{
	"address": "addr1qryq74hega5juqdl9n86697mwx0mfxu5hf02vuphnqfejtj3rkjw6sd4xtj3ka2c0wsvul94apvycmhy3lr2dmne6w8seyfa3d",
	"amount": { "quantity": 1000000, "unit": "lovelace" }
	}
	],
  "metadata":{ "4": { "map": [ { "k": { "string": "key" }, "v": { "string": "value" } }, { "k": { "string": "14" }, "v": { "int": 42 } } ] } },
	"passphrase": "Secure Passphrase"
	}'
 ```
 - delegate to a stake-pool
 ```bash
$ curl -vX PUT http://localhost:8090/v2/stake-pools/6f79ce9538fb79c9bb4ccd7a290eb58a878188b92fb97a93c44922baf68abb7d/wallets/617963656a409b8a6828dc3a09001de22af90400 \
	-H "Content-Type: application/json; charset=utf-8" \
	-d '{
			"passphrase": "Secure Passphrase"
			}'
 ```
 - network information, parameters, clock
 ```bash
$ curl -vX GET http://localhost:8090/v2/network/information
$ curl -vX GET http://localhost:8090/v2/network/parameters
$ curl -vX GET http://localhost:8090/v2/network/clock
 ```

## Start current version of the wallet

1. Get **current** release -> https://github.com/cardano-foundation/cardano-wallet/releases
2. start wallet (on the same `--database`)

```bash
$ cardano-wallet serve --port 8090 \
		--node-socket ../relay1/node.socket \
		--testnet testnet-byron-genesis.json  \
		--database ./wallet-db
```

3. Wallet starts and functions properly.

4. Check basic functionality:
 - add shelley wallet
 - add byron wallet
 - make sure sync progress and block height are tracked on both syncing and synced wallets
 - send tx
 - delegate to a stake-pool
 - network information, parameters, clock
