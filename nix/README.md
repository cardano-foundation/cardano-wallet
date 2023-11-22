# Nix dependencies

We use the [nix flake][flake] format to manage package dependencies
and present our software as a reuseable package.

See also [Contributor manual – Nix build language](https://cardano-foundation.github.io/cardano-wallet/contributor/what/nix.html).

The file `./flake.nix` contains the main package description.
The `./nix/` directory contains various helper functions and utilities.

  [flake]: https://nixos.wiki/wiki/Flakes
  [haskell.nix]: https://github.com/input-output-hk/haskell.nix


## Basic usage of a nix flake

* Building an attribute for your current system

    ```
    $ nix build .#cardano-wallet
    ```

* Building a derivation for a specific system, e.g. `x86_64-linux`

    ```
    $ nix build .#packages.x86_64-linux.cardano-wallet
    ```

* Discover contents of the flake

    ```
    $ nix repl
    Welcome to Nix 2.13.3. Type :? for help.

    nix-repl> :lf .
    Added 21 variables.

    nix-repl> outputs. [PRESS TAB]
    outputs.apps            outputs.devShells       outputs.overlay
    outputs.checks          outputs.legacyPackages  outputs.packages
    outputs.ci              outputs.mkApp
    outputs.defaultApp      outputs.nixosModule
    outputs.defaultPackage  outputs.nixosModules
    ```

* Development shell
    ```
    $ nix develop
    ```

* On macOS: Avoiding segmentation fault due to bug in garbage collector

    ```
    $ GC_DONT_GC=1 nix develop
    ```

## External documentation

* [Nix flakes][flake]

* [Haskell.nix][]
    * Basic example with cross-compilation: https://github.com/input-output-hk/haskell-nix-example
    * Setting up the binary cache to avoid recompiling GHC: https://input-output-hk.github.io/haskell.nix/tutorials/getting-started#setting-up-the-binary-cache