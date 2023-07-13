# Upgrading the GHC version of cardano-wallet

Here is a reference PR that upgrades to GHC 8.10.7: https://github.com/cardano-foundation/cardano-wallet/pull/2969

**WARNING**: Updating haskell.nix and/or GHC changes a lot of the build environment. You should expect to spend time fixing breakages.

## Process

- Update "with-compiler" in cabal.project:
```diff
diff --git a/cabal.project b/cabal.project
index 1ba2edf625..109534719a 100644
--- a/cabal.project
+++ b/cabal.project
@@ -39,7 +39,7 @@
 --------------------------------------------------------------------------------

 index-state: 2021-10-05T00:00:00Z
-with-compiler: ghc-8.10.5
+with-compiler: ghc-8.10.7

 packages:
     lib/wallet/
```

- Update haskell.nix `nix flake lock --update-input haskellNix`.

## Troubleshooting

The following is a list of issues encountered so far while executing this process:

### Compile-time error when building a package or dependency

For example:

```
src/Cardano/Config/Git/Rev.hs:33:35: error:
    • Exception when trying to run compile-time code:
        git: readCreateProcessWithExitCode: posix_spawnp: failed (Undefined error: 0)
      Code: gitRevFromGit
    • In the untyped splice: $(gitRevFromGit)
   |
33 |         fromGit = T.strip (T.pack $(gitRevFromGit))
```

In this case, the first guess is that git is not in the PATH when compiling the Template Haskell of cardano-config. The following line in our nix/haskell.nix file fixes this, adding a build-time dependency to our downstream dependencies (and one of our own too):

```diff
diff --git a/nix/haskell.nix b/nix/haskell.nix
index 8bb80f7e99..8ac227a865 100644
--- a/nix/haskell.nix
+++ b/nix/haskell.nix
@@ -302,6 +302,10 @@ let
  pkg-set = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
       # ...
+      {
+        packages.cardano-wallet.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
+        packages.cardano-config.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
+      }
     ];
   };
```
