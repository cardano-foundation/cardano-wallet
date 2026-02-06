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

## Windows Cross-Compilation

Windows builds are produced using cross-compilation from Linux.

### Building for Windows

```
$ nix build .#ci.artifacts.win64.release
```

### How it works

Windows cross-compilation uses [haskell.nix][] with GHC's `x86_64-w64-mingw32` target.
Template Haskell code is compiled using `iserv-proxy`, which runs the Windows
executable under Wine to evaluate TH splices.

### Configuration

Windows-specific build fixes are in `nix/haskell.nix`, in the section:

```nix
# Windows cross-compilation fixes (from cardano-node)
({ lib, pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows { ... })
```

This includes:
- **unix-compat**: Removes reference to `msvcrt` library
- **unix-time**: Removes reference to `mingwex` library
- **crypton-x509-system**: Fixes case sensitivity (`Crypt32` → `crypt32`)
- **streaming-commons**: Clears outdated patch (already applied upstream)

### iserv-proxy: Deep Dive

#### What is iserv-proxy?

Template Haskell (TH) needs to execute code at compile time. When cross-compiling,
the host (Linux) can't run target (Windows) binaries directly. GHC solves this
with **iserv** (interpreter server):

```
┌─────────────────────────────────────────────────────────────────┐
│ Linux Host                                                       │
│                                                                  │
│  ┌──────────┐     TCP/IP      ┌─────────────────────────────┐   │
│  │   GHC    │◄───────────────►│    Wine + iserv-proxy.exe   │   │
│  │ compiler │     (socket)    │    (runs Windows binary)    │   │
│  └──────────┘                 └─────────────────────────────┘   │
│       │                                   │                      │
│       │ compile TH                        │ evaluate TH          │
│       ▼                                   ▼                      │
│  .o files                           execute splices              │
└─────────────────────────────────────────────────────────────────┘
```

1. GHC compiles code containing Template Haskell
2. When it hits a TH splice, it sends the code to iserv-proxy via a socket
3. iserv-proxy (running under Wine) executes the Windows binary
4. Results are sent back to GHC over the socket
5. GHC incorporates the result into the compiled output

#### Why network version matters

The `network` Haskell library versions affect iserv-proxy:

| network version | Socket options used | Wine support |
|-----------------|---------------------|--------------|
| < 3.2.8.0       | Standard socket opts | Works        |
| ≥ 3.2.8.0       | `IP_DONTFRAG`, `IP_MTU_DISCOVER` | **Fails** |

Wine doesn't implement `IP_DONTFRAG`/`IP_MTU_DISCOVER`, causing:
```
iserv-proxy-interpreter.exe: Network.Socket.setSockOpt: failed (WSAENOPROTOOPT)
```

#### Our fix

The `iserv-proxy` input in `flake.nix` is overridden to use `stable-haskell/iserv-proxy`
which constrains `network < 3.2.8.0`:

```nix
# flake.nix
inputs.iserv-proxy = {
  url = "github:stable-haskell/iserv-proxy?ref=iserv-syms";
  flake = false;
};
inputs.haskellNix = {
  url = "github:input-output-hk/haskell.nix";
  inputs.iserv-proxy.follows = "iserv-proxy";  # override haskell.nix's iserv-proxy
};
```

#### Maintaining iserv-proxy compatibility

When haskell.nix updates, check if their iserv-proxy input changes. If Wine support
for the problematic socket options improves, the constraint can be relaxed.

**Verify current network version**:
```
$ nix path-info -r .#legacyPackages.x86_64-linux.haskell-nix.iserv-proxy-interpreter.components.exes.iserv-proxy-interpreter | grep network
```

Expected output should show `network-3.2.7.0` or earlier, not `3.2.8.0`.

### Troubleshooting Windows builds

**Build with verbose sequential output** (helps identify which package fails):
```
$ nix build .#ci.artifacts.win64.release -j1 -L 2>&1 | tee win-build.log
```

**Find the failing package** in the log:
```
$ grep -E "(error:|FAILED|Reason: builder failed)" win-build.log
```

**Check for patch conflicts** (common issue when haskell.nix patches are outdated):
```
$ grep -B5 "Reversed (or previously applied) patch" win-build.log
```

**Fix patch conflicts**: Add to the Windows section in `nix/haskell.nix`:
```nix
packages.PACKAGE-NAME.patches = lib.mkForce [];
# If the patch was fixing something still needed:
packages.PACKAGE-NAME.postPatch = ''
  sed -i 's/OLD/NEW/g' file.cabal
'';
```

**Check iserv-proxy version** (should use network < 3.2.8.0):
```
$ nix path-info -r .#legacyPackages.x86_64-linux.haskell-nix.iserv-proxy-interpreter.components.exes.iserv-proxy-interpreter | grep network
```

## External documentation

* [Nix flakes][flake]

* [Haskell.nix][]
    * Basic example with cross-compilation: https://github.com/input-output-hk/haskell-nix-example
    * Setting up the binary cache to avoid recompiling GHC: https://input-output-hk.github.io/haskell.nix/tutorials/getting-started#setting-up-the-binary-cache
