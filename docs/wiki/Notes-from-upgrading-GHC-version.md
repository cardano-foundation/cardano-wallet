# Upgrading the GHC version of cardano-wallet

Here is a reference PR that upgrades to GHC 8.10.7: https://github.com/input-output-hk/cardano-wallet/pull/2969

**WARNING**: Updating haskell.nix, the stackage LTS, and/or GHC, changes a lot of the build environment. You should expect to spend time fixing breakages.

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
     lib/core/
```

- Update "compiler-nix-name" in nix/overlays/build-tools

```diff
diff --git a/nix/overlays/build-tools.nix b/nix/overlays/build-tools.nix
index 28c46f2e4df..6b0778eb5d0 100644
--- a/nix/overlays/build-tools.nix
+++ b/nix/overlays/build-tools.nix
@@ -41,7 +41,7 @@ let
     weeder.version                  = "2.1.3";
   };
 
-  compiler-nix-name = "ghc8105";  # TODO: get it from the project
+  compiler-nix-name = "ghc8107";  # TODO: get it from the project
 in
 
 pkgs: super: let
```

- Update compiler in stack.yaml. Update the resolver too if necessary (e.g. to get a newer set of packages):

```diff
diff --git a/stack.yaml b/stack.yaml
index 18b0916933..29f2d6e961 100644
--- a/stack.yaml
+++ b/stack.yaml
@@ -13,8 +13,8 @@
 #
 ########################################################################
 
-resolver: lts-18.5
-compiler: ghc-8.10.5
+resolver: lts-18.13
+compiler: ghc-8.10.7
 
 packages:
 - lib/core
```
- Update haskell.nix `niv update haskell.nix -b master`.
- Run nix/regenerate.sh.

## Troubleshooting

The following is a list of issues encountered so far while executing this process:

### nix/regenerate.sh fails

For example:

```
error: assertion ((final).buildPackages.haskell-nix.compiler."${compiler-nix-name'}".version == (final).buildPackages.haskell-nix.compiler."${(((plan-pkgs).pkgs  hackage)).compiler.nix-name}".version) failed at /nix/store/zs00ba6w5972g0sy95r89z4nzbm16kqi-haskell.nix-src/overlays/haskell.nix:152:15
```

This particular issue was caused by a mismatch in the GHC versions of the Nix expressions updated above, and the GHC version listed in the materialized (cached) files. Regenerate should change the cached files, but instead was failing with the above error.

To fix this, I had to:

- Navigate to overlays/build-tools.nix, and change the following lines:

```diff
diff --git a/nix/overlays/build-tools.nix b/nix/overlays/build-tools.nix
index 28c46f2e4..3e80caad4 100644
--- a/nix/overlays/build-tools.nix
+++ b/nix/overlays/build-tools.nix
@@ -50,7 +50,7 @@ pkgs: super: let
   mkTool = name: args: pkgs.haskell-nix.hackage-package ({
     inherit name index-state compiler-nix-name;
   } // pkgs.lib.optionalAttrs enableMaterialization {
-    checkMaterialization = false;
+    checkMaterialization = true;
     materialized = ../materialized + "/${name}";
   } // builtins.removeAttrs args ["exe"]);
```

Re-run nix/regenerate.sh and it should fail with an error that contains an `updateMaterialized` script to run. Run that script, and return `checkMaterialization` to `false`.

Alternatively, running `s/ghc8105/ghc8107/`, then re-running nix/regenerate.sh, would have done the trick.

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
+        packages.cardano-wallet-core.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
+        packages.cardano-config.components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
+      }
     ];
   };
```
