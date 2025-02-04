# **Manage dependencies versions in Cabal files :: AP**

|         |            |
|---------|------------|
| Started | 2025-02-03 |
| Decided |            |

## **Why**

In order to increase the number of scenarios in which the cardano-wallet code could be useful, we want to be able to easily publish libraries such as [coin-selection](https://github.com/cardano-foundation/cardano-wallet/blob/a9761ea150ac6d0d52b255098f3f38b3cf9a07b6/lib/coin-selection/cardano-coin-selection.cabal#L1) or [balance-tx](https://github.com/cardano-foundation/cardano-wallet/blob/a9761ea150ac6d0d52b255098f3f38b3cf9a07b6/lib/balance-tx/cardano-balance-tx.cabal#L1) to [CHaP](https://github.com/IntersectMBO/cardano-haskell-packages), or even [Hackage](https://hackage.haskell.org) whenever possible.

Currently, the cardano-wallet codebase manages downstream constraints at the toplevel, in the [cabal.project](https://github.com/cardano-foundation/cardano-wallet/blob/a9761ea150ac6d0d52b255098f3f38b3cf9a07b6/cabal.project#L174) file, following the constraints of the Cardnao libraries it depends on. While this is fine for an application, it does not work for publishing libraries because of the way Cabal and package repositories work.

## **Decision**

* We set version bounds constraints at the level of individual `.cabal` files instead of the `.project` file
* We update version bounds as needed, tracking upstream Cardano dependencies as they are released to _CHaP_
* We publish useful libraries to CHaP regularly, tracking upstream dependencies
* We monitor the use of those libraries

## **Details**

* All components of the cardano-wallet are currently versioned using `0.yyyy.mm.dd` pattern. In order to ensure published libraries are deemed newer, we need to change their versioning scheme to the usual `1.x.y.z` (eg. following [PVP](https://pvp.haskell.org))
* We need to start from the bottom-up with [cardano-wallet-read](https://github.com/cardano-foundation/cardano-wallet-agda/blob/f9709fafae2585f24f9833e71bf0b4de3023b3ed/lib/cardano-wallet-read/cardano-wallet-read.cabal#L1) package which lives in the cardano-wallet-agda repository
* User-facing libraries might depend on "technical" libraries which are mostly useless and implementation details. We nevertheless need to publish those packages to CHaP in order for Cabal resolution mechanism to work properly
* In order to limit the maintenance burden, version bound constraints should be set only on _public libraries_. Cabal will "naturally" propagate those constraints to other packages
* We acknowledge that defining good version constraints is a bit of a dark art, so our approach will be to start with a rather restrictive set and expand through _package revisions_ as needed
* We don't invest time in automation and tooling initially, setting and managing bounds manually, until we reach a point where it's becoming overwhelming
  * It's expected this issue will crop up if and only if users start using those published packages and submit issues
  * There exists some tools ([cabal-extras](https://github.com/phadej/cabal-extras), [cabal-bounds](https://hackage.haskell.org/package/cabal-bounds), [cabal gen-bounds](https://cabal.readthedocs.io/en/3.4/cabal-package.html), `cabal outdated`...) but perhaps another tool would be helpful for others

## **Rationale**

* This relates to a feature that's been [requested by users](https://github.com/cardano-foundation/cardano-wallet/issues/4411)
