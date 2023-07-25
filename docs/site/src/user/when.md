# When to use `cardano-wallet`

Cardano-wallet was originally designed to provide the wallet logic for the graphical frontend [Daedalus][]. Cardano-wallet does not offer a graphical interface, but it provides an HTTP API and a command line interface (CLI).

Cardano-wallet is a **full-node wallet**, which means that it depends on a `cardano-node` to provide blockchain data. In turn, this means that `cardano-wallet` enjoys the security properties of the Ouroboros consensus protocol. Other wallets such as [Eternl][], [Yoroi][] or [Lace][] are light wallets, which means that they have to trust a third party to provide blockchain data, but they use less computing resources in return.

Cardano-wallet supports

* All use cases

    * Managing a balance of ADA and tokens
    * Submitting transactions to the Cardano network via a local `cardano-node``

* Personal use

    * Staking
    * Compatibility with legacy wallets
    * Basic privacy with payment addresses creation

* Business use

    * Minting and burning tokens
    * Multi-party signatures

We also accomodate

* Cryptocurrency Exchanges

  [Daedalus]: https://daedaluswallet.io/
  [Eternl]: https://eternl.io/
  [Lace]: https://www.lace.io/
  [Yoroi]: https://yoroi-wallet.com/

Please [reach out to us][forum] if you feel that we could do a better job of covering your use case. Preferably, tell us more about what you are trying to achieve and the problems you want to solve — then we can help you find out whether `cardano-wallet` is the right tool for you, and we can better accomodate your use case in our development roadmap.

  [forum]: https://github.com/cardano-foundation/cardano-wallet/discussions

## Scalability

### Computing resources

A single `cardano-wallet` process supports muliple wallets, each with separate (sets of) signing keys. These wallets run fairly independently, so that the computing resources used by `cardano-wallet` will be the sum of the resources used by each wallet.

Each wallet uses computing resources that grow with the number of addresses and UTxO in the wallet. For precise numbers, see our [Hardware Recommendations](user/hardware-recommendations.md).

If computing resources on a single machine are not sufficient, you can run multiple `cardano-wallet` process with different wallets on separate machines.

### Transaction throughput

If you want to submit frequent transactions to the Carano blockchain, the wallet is typically not a bottleneck — rather, the transactions per seconds that can be accepted by the Cardano blockchain is the limiting factor. That said, transactions per second are not directly relevant to your end-users, see [Performance engineering: Lies, damned lies and (TPS) benchmarks][neil] for a thorough discussion of performance on Cardano.

In general, note that the more your transactions depend on each other, the less they can be processed in parellel, reducing throughput and increasing latency.

On Cardano mainnet, on average 1 block per 20 slots is made, and one slot lasts 2 seconds (parameters as of July 2023). Each [block may contain a maximum number of transactions][block-size]. Using these quantities, you can estimate an upper bound on the number of your transactions per second that the blockchain can accomodate.

If you need more frequent transactions that the estimate above, you need to consider a scaling solution such as [Hydra][] or a sidechain.

  [block-size]: https://cardano.stackexchange.com/questions/216/is-there-a-maximum-number-of-transactions-a-block-can-hold 
  [Hydra]: https://hydra.family/
  [neil]: https://www.youtube.com/watch?v=gpSnyCn2s9U
