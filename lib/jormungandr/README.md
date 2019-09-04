# Jörmungandr backend

Jörmungandr is a node implementation, written in rust, with the initial aim
to support the Ouroboros type of consensus protocol.
See [link](https://input-output-hk.github.io/jormungandr/) to get more information.

Instructions how to install Jörmungandr are [here](https://github.com/input-output-hk/jormungandr).
After the installation one should see the below :

``` bash
$ jcli --version
jcli 0.3.3
$ jormungandr --version
jormungandr 0.3.3
```

# Testing

``` bash
$ stack test cardano-wallet-jormungandr:integration
$ stack test cardano-wallet-jormungandr:unit
```

For manual testing try :

``` bash
$ pwd
.../cardano-wallet
$ stack build
$ cardano-wallet-jormungandr launch --genesis-block lib/jormungandr/test/data/jormungandr/block0.bin --bft-leaders lib/jormungandr/test/data/jormungandr/secret.yaml
```

Here we are taking genesis block and bft leaders configuration as defined in integration tests.
For more options invoke help:

``` bash
$ stack exec -- cardano-wallet-jormungandr launch --help
```

If the cardano-wallet part talking with Jörmungandr is running, then in the separate console
call the api, for example:

``` bash
$ curl -H "Content-Type: application/json" http://localhost:8090/v2/wallets
[]
```
