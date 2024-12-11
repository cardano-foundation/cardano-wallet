# Release Process

 - Create new page on cardano-wallet's [wiki](https://github.com/cardano-foundation/cardano-wallet/wiki/_new) called `Release vYYYY-MM-DD` by copying contents of the previous releas.
 - Follow the `Release checklist`. Update progress or report obstacles on the thread.

## Releasing packages to CHaP

[CHaP](https://github.com/IntersectMBO/cardano-haskell-packages) or the _Cardano Haskell Packages_ repository is useful to publish _Haskell_ packages relevant to the Cardano ecosystem as this allows standard build tools like `Cabal` to work out-of-the-box with only some configuration to point to the repository in addition to https://hackage.haskell.org

To publish packages on CHaP:

1. Clone the repository locally from https://github.com/cardano-foundation/cardano-haskell-packages/
2. Run script to create relevant commits, for example to release `cardano-coin-selection` package and its (local) dependencies:

   ```
   $ scripts/add-from-github.sh https://github.com/cardano-foundation/cardano-wallet 9eb5f59c328163ca061a20f47519686b6f118d74 lib/coin-selection lib/primitive lib/test-utils lib/delta-types lib/crypto-primitives lib/launcher lib/numeric lib/text-class
   ```
3. Open a Pull Request on upstream repository (ie. https://github.com/IntersectMBO/cardano-haskell-packages/)
