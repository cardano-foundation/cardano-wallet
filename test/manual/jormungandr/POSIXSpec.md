# Cardano.Launcher.POSIX

## OS

Windows, MacOS, Linux

## Goal

By default, the Haskell runtime and the `process` library we use to spawn new
processes in the wallet don't catch `SIGTERM` signals on a unix system. This
means that if we kill the wallet process, the sub-processes it has spawned
will endure.

What follows are steps which explains how to test that `SIGINT` and `SIGTERM`
signals are correctly handled on a unix system.

## Preliminaries
### Start the wallet

```
# Normally
$ stack exec -- cardano-wallet-jormungandr launch --genesis-block lib/jormungandr/test/data/jormungandr/block0.bin -- --secret lib/jormungandr/test/data/jormungandr/secret.yaml
# In the background:
$ stack exec -- cardano-wallet-jormungandr launch --genesis-block lib/jormungandr/test/data/jormungandr/block0.bin -- --secret lib/jormungandr/test/data/jormungandr/secret.yaml &
```

### Check pid / if running

To check if the wallet and jormungandr are running, and to find their pids, the following
command can be used:

```
$ ps -ef | grep " jormungandr "; ps -ef | grep " cardano-wallet-jormungandr "

  501 73924 73923   0  6:14PM ttys006    0:00.40 jormungandr --genesis-block lib/jormungandr/test/data/jormungandr/block0.bin --rest-listen 127.0.0.1:55218 --storage /Users/Johannes/.local/share/cardano-wallet/jormungandr/testnet/chain --secret lib/jormungandr/test/data/jormungandr/secret.yaml
  501 73923   623   0  6:14PM ttys006    0:00.67 cardano-wallet-jormungandr launch --genesis-block lib/jormungandr/test/data/jormungandr/block0.bin -- --secret lib/jormungandr/test/data/jormungandr/secret.yaml
```

In the example both are running. The wallet with a pid of 73923, and
jormungandr with a pid of 73924.

## Steps

### `SIGINT`

- Start the wallet.
- In another terminal, check that both processes are running (the wallet and jormungandr)
- Send a `SIGINT` signal using by pressing `CTRL-C` in the first terminal
- Check that both processes are no longer running.


### `SIGQUIT`

- Start the wallet .
- In another terminal, check that both processes are running (the wallet and jormungandr)
- Send a `SIGQUIT` signal using by pressing `CTRL-\` in the first terminal
- Check that both processes are no longer running.

### `SIGTERM`

- Start the wallet in background
- Check that both processes are running (the wallet and jormungandr)
  and note the pid of the wallet.
- Send a `SIGTERM` signal to the pid of the wallet using `kill <pid>`
- Check that both processes are no longer running.


### `SIGKILL`

> **Disclaimer**
>
> The semantic of `SIGKILL` doesn't allow us to do any clean-up after receiving
> it (we can't shove a signal handler for a `SIGKILL`!). So, if one kills the
> wallet, one doesn't kill the sub-processes the wallet has started. This
> should be handled at the OS level, or via different mechanism (like, having a
> shared socket between the wallet and its children).
>
> As a consequence, sending a `SIGKILL` to the wallet will NOT terminate the
> sub-processes started by the wallet. This is, for now, a known limitation.
>
> Please note that steps below are therefore currently invalid!

- Start the wallet in background
- Check that both processes are running (the wallet and jormungandr)
  and note the pid of the wallet.
- Send a `SIGKILL` signal to the pid of the wallet using `kill -9 <pid>`
- Run `ps -ef | grep cardano`, and control that the wallet isn't running
  anymore. Jormungandr should however still be up-and-running.
