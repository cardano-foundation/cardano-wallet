# Jörmungandr backend

Jörmungandr is a node implementation, written in Rust, with the initial aim
of supporting the Ouroboros type of consensus protocol.
See [link](https://input-output-hk.github.io/jormungandr/) to get more information.

Instructions how to install Jörmungandr are [here](https://github.com/input-output-hk/jormungandr).
After the installation, one can follow the steps below:

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

For manual testing, try :

``` bash
$ pwd
.../cardano-wallet
$ stack build
$ cardano-wallet-jormungandr launch --genesis-block lib/jormungandr/test/data/jormungandr/block0.bin -- --secret lib/jormungandr/test/data/jormungandr/secret.yaml
```

Here we are using a genesis block and secrets as defined in the integration tests.
For more options, see the help text:

``` bash
$ stack exec -- cardano-wallet-jormungandr launch --help
```

If the `cardano-wallet` server is running with a Jörmungandr backend, then in a separate console
call the API. For example:

``` bash
$ curl -H "Content-Type: application/json" http://localhost:8090/v2/wallets
[]
```
