# Public Sublibrary Repro

This directory contains a standalone `haskell.nix` repro for the public
sublibrary issue described in `input-output-hk/haskell.nix#1662`.

It has two tiny consumer packages:

- `hackage/`: depends on `multi-except:semigroupoid-instances` from Hackage
- `chap/`: depends on `io-classes:si-timers` from CHaP

Expected workflow:

```bash
CACHE_FLAGS=(
  --option extra-substituters https://cache.iog.io
  --option extra-trusted-public-keys hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
)

nix "${CACHE_FLAGS[@]}" build .#hackage
nix "${CACHE_FLAGS[@]}" develop .#hackage -c bash -lc 'cd hackage && cabal build all'

nix "${CACHE_FLAGS[@]}" build .#chap
nix "${CACHE_FLAGS[@]}" develop .#chap -c bash -lc 'cd chap && cabal build all'
```

If the bug reproduces, `nix build` succeeds while `cabal build` inside the
matching dev shell rejects the package because the installed package DB does
not expose the requested public sublibrary.
