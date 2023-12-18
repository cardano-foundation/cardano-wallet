## Hardware recommendations

As cardano-wallet runs side by side with cardano-node, the hardware requirements for cardano-wallet would largely depend on the hardware requirements for cardano-node. Current hardware requirements for cardano-node are published on cardano-node's [release page](https://github.com/IntersectMBO/cardano-node/releases). For most cases cardano-node's hardware requirements are good enough to cover requirements of running cardano-wallet as well.

Here are some general hardware recommendations for running cardano-wallet:

*  **Processor:** A multicore processor with a clock speed of at least 1.6 GHz or faster is recommended, but a faster processor is better for optimal performance.
*  **Memory:** A minimum of 4 GB of RAM is recommended, although more is better.
*  **Storage:** A minimum of 5 GB of free storage for wallet data is recommended.
*  **Network:** A stable internet connection with at least 5 Mbps upload and download speeds is recommended for syncing the blockchain data and performing transactions.

Again, these are general recommendations and the actual hardware requirements may vary depending on factors such as the number and size of wallets being managed and the specific usage patterns of the software. In particular the above requirements are good enough to handle fairly large wallet having **15k addresses** and **15k transactions** in history. Smaller wallets would not require as much resources but larger would require more.
