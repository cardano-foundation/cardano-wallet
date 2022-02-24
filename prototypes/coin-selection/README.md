# CoinSelection module API mock-up

This is an attempt to come up with the simplest possible interface for coin selection that would work, without using concrete types.

It will hopefully help us define a common understanding of what the coin selection module will look like after the refactors are done.

## Building and viewing

The best way to review this mock-up is by reading the Haddock API documentation. So:

```console
$ cd prototypes
$ cabal haddock all
$ xdg-open $(find dist-newstyle -wholename '*coin-selection*' -name index.html)
```
