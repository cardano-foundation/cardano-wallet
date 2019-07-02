# Cardano.Launcher.POSIX

## Goal

By default, the Haskell runtime and the `process` library we use to spawn new
processes in the launcher don't catch `SIGTERM` signals on a unix system. This
means that if we kill the launcher process, the sub-processes it has spawned
will endure.

What follows are steps which explains how to test that `SIGINT` and `SIGTERM`
signals are correctly handled on a unix system.

## Steps

### `SIGINT`

- Start the launcher using `stack exec -- cardano-wallet launch`
- In another terminal, run `ps -ef | grep cardano`, there should be three
  processes running (the launcher, the wallet and the underlying chain
  producer)
  ```
  $ ps -ef | grep cardano
  10983  4904  0 09:42 pts/1 00:00:00 /home/user/Documents/IOHK/cardano-wallet/.stack-work/install/x86_64-linux/lts-13.8/8.6.3/bin/cardano-wallet
  11003 10983 75 09:42 pts/1 00:03:30 cardano-http-bridge start --port 8080
  11071 10983 58 09:42 pts/1 00:02:41 NETWORK=mainnet cardano-wallet serve --port 8090 --node-port 8080
  ```

- Send a `SIGINT` signal using by pressing `CTRL-C` in the first terminal
- Run `ps -ef | grep cardano`, there's no more process running: the launcher,
  the wallet and the chain producer have all stopped.


### `SIGQUIT`

- Start the launcher using `stack exec -- cardano-wallet launch`
- In another terminal, run `ps -ef | grep cardano`, there should be three
  processes running (the launcher, the wallet and the underlying chain
  producer)
  ```
  $ ps -ef | grep cardano
  10983  4904  0 09:42 pts/1 00:00:00 /home/user/Documents/IOHK/cardano-wallet/.stack-work/install/x86_64-linux/lts-13.8/8.6.3/bin/cardano-wallet
  11003 10983 75 09:42 pts/1 00:03:30 cardano-http-bridge start --port 8080
  11071 10983 58 09:42 pts/1 00:02:41 NETWORK=mainnet cardano-wallet serve --port 8090 --node-port 8080
  ```

- Send a `SIGQUIT` signal using by pressing `CTRL-\` in the first terminal
- Run `ps -ef | grep cardano`, there's no more process running: the launcher,
  the wallet and the chain producer have all stopped.


### `SIGTERM`

- Start the launcher in background using `stack exec -- cardano-wallet launch &`
- Run `ps -ef | grep cardano` and lookup the `pid` of the launcher process (and
  control that there are indeed three processes running: the launcher, the
  wallet and the chain producer)
  ```
  $ ps -ef | grep cardano
  10983  4904  0 09:42 pts/1 00:00:00 /home/user/Documents/IOHK/cardano-wallet/.stack-work/install/x86_64-linux/lts-13.8/8.6.3/bin/cardano-wallet
  11003 10983 75 09:42 pts/1 00:03:30 cardano-http-bridge start --port 8080
  11071 10983 58 09:42 pts/1 00:02:41 NETWORK=mainnet cardano-wallet serve --port 8090 --node-port 8080
  ```
- Send a `SIGTERM` signal to this pid using `kill <pid>`
- Run `ps -ef | grep cardano`, there's no more process running: the launcher,
  the wallet and the chain producer have all stopped.


### `SIGKILL`

> **Disclaimer**
>
> The semantic of `SIGKILL` doesn't allow us to do any clean-up after receiving
> it (we can't shove a signal handler for a `SIGKILL`!). So, if one kills the
> launcher, one doesn't kill the sub-processes the launcher has started. This
> should be handled at the OS level, or via different mechanism (like, having a
> shared socket between the launcher and its children).
>
> As a consequence, sending a `SIGKILL` to the launcher will NOT terminate the
> sub-processes started by the launcher. This is, for now, a known limitation.
>
> Please note that steps below are therefore currently invalid!

- Start the launcher in background using `stack exec -- cardano-wallet launch &`
- Run `ps -ef | grep cardano` and lookup the `pid` of the launcher process (and
  control that there are indeed three processes running: the launcher, the
  wallet and the chain producer)
  ```
  $ ps -ef | grep cardano
  10983  4904  0 09:42 pts/1 00:00:00 /home/user/Documents/IOHK/cardano-wallet/.stack-work/install/x86_64-linux/lts-13.8/8.6.3/bin/cardano-wallet
  11003 10983 75 09:42 pts/1 00:03:30 cardano-http-bridge start --port 8080
  11071 10983 58 09:42 pts/1 00:02:41 NETWORK=mainnet cardano-wallet serve --port 8090 --node-port 8080
  ```
- Send a `SIGKILL` signal to this pid using `kill -9 <pid>`
- Run `ps -ef | grep cardano`, and control that the launcher isn't running
  anymore. The wallet server and the underlying chain producer should however
  still be up-and-running.
