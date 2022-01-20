# Plutus E2E generation

## Why doesn't this code live in the base project?

- One of the key benefits of this code is being able to use the same language as plutus-apps to define transactions.
- From a dependency point of view, `cardano-wallet-core-integration` can depend on plutus-apps. Thus, the code here could live in `cardano-wallet-core-integration` instead.
- However, when we enter a shell, we don't just enter a shell for
  `cardano-wallet-core-integration`, but rather a shell for the entire project.
  - If any dependency of our project packages relies on a project package, Haskell.nix removes that dependency from the shell.
  - `cardano-wallet-core` is a dependency of `plutus-ledger`, and `plutus-ledger` is a common dependency of all plutus packages.
  - Thus all plutus packages are stripped from the shell and we can't hack on `cardano-wallet-core-integration`.

To get around this issue, we create a separate tool here that simply generates the transactions using `plutus` packages, then serializes those transactions in a format suitable for `cardano-wallet-core-integration`.
