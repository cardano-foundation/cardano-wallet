# Quickstart: Verify Unit Test Memory Bounds

Run commands from the issue worktree inside the Nix development environment.

## Focused deterministic regression

```bash
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options='--match="/Control.Monad.UtilSpec/applyNM/prop_applyNM_iterate @[] @Int/" --seed=1 -j1 +RTS -M256M -N1 -s -RTS'
```

Before Slice 1 this exits with heap exhaustion. After Slice 1 it completes one
example with zero failures under the same 256 MiB cap.

## Inspect compiled runtime defaults

For each affected component, obtain its binary with `cabal list-bin` and run:

```bash
<test-binary> +RTS --info | grep 'Flag -with-rtsopts'
```

After Slice 2 the value is `-M2G -N4` for all six affected test executables.

## Monolithic acceptance smoke

```bash
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options='+RTS -s -RTS'
```

The executable's compiled defaults enforce the 2 GiB heap ceiling and four RTS
capabilities. Success is exit 0 with no heap exhaustion or OS-level OOM kill.
