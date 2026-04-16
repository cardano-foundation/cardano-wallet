# Investigation: `cabal build` vs preinstalled public sublibraries

This note captures the current diagnosis for the `cardano-wallet` dev-shell
regression around public Cabal sublibraries.

Date: 2026-04-16

This is a `cardano-wallet` issue note even though the final fix will likely
land upstream in `cabal-install` and possibly partly in `haskell.nix`.

## External references

- One-file repro gist:
  - <https://gist.github.com/paolino/9ad5f5ff90a665018f8798aa32eaf7e2>
- `haskell.nix` exact commit:
  - <https://github.com/input-output-hk/haskell.nix/commit/ef52c36b9835c77a255befe2a20075ba71e3bfab>
- `haskell.nix` pinned bad revision:
  - <https://github.com/input-output-hk/haskell.nix/tree/ef52c36b9835c77a255befe2a20075ba71e3bfab>
- `haskell.nix` shell builder:
  - <https://github.com/input-output-hk/haskell.nix/blob/ef52c36b9835c77a255befe2a20075ba71e3bfab/builder/shell-for.nix>
- `haskell.nix` config generation:
  - <https://github.com/input-output-hk/haskell.nix/blob/ef52c36b9835c77a255befe2a20075ba71e3bfab/builder/make-config-files.nix>
- `haskell.nix` component builder:
  - <https://github.com/input-output-hk/haskell.nix/blob/ef52c36b9835c77a255befe2a20075ba71e3bfab/builder/comp-builder.nix>
- `cabal-install` installed-package solver conversion:
  - <https://github.com/haskell/cabal/blob/b748a112714dc765029654a994ca38f6682c7d4b/cabal-install-solver/src/Distribution/Solver/Modular/IndexConversion.hs>
- `cabal` exact commit:
  - <https://github.com/haskell/cabal/commit/b748a112714dc765029654a994ca38f6682c7d4b>
- `cabal-install` pre-existing package representation:
  - <https://github.com/haskell/cabal/blob/b748a112714dc765029654a994ca38f6682c7d4b/cabal-install-solver/src/Distribution/Solver/Types/InstSolverPackage.hs>
- `cabal-install` configured conversion:
  - <https://github.com/haskell/cabal/blob/b748a112714dc765029654a994ca38f6682c7d4b/cabal-install-solver/src/Distribution/Solver/Modular/ConfiguredConversion.hs>
- `cabal-install` project planning:
  - <https://github.com/haskell/cabal/blob/b748a112714dc765029654a994ca38f6682c7d4b/cabal-install/src/Distribution/Client/ProjectPlanning.hs>
- Cabal installed package metadata:
  - <https://github.com/haskell/cabal/blob/b748a112714dc765029654a994ca38f6682c7d4b/Cabal-syntax/src/Distribution/Types/InstalledPackageInfo.hs>
- Cabal installed package index:
  - <https://github.com/haskell/cabal/blob/b748a112714dc765029654a994ca38f6682c7d4b/Cabal/src/Distribution/Simple/PackageIndex.hs>

## Executive summary

There are two distinct problems:

1. `haskell.nix` `shellFor` has a shell bug for `ghc`/`ghci`.
2. `cabal-install` has a deeper bug for `cabal build` when it tries to reuse
   public sublibraries from a pre-existing package database.

These are related but not identical.

The important conclusion is:

- `nix build` succeeds because it bypasses the broken `cabal-install` solver
  path and passes exact `--dependency=pkg:sublib=unit-id` flags.
- `cabal build` in the dev shell fails because it re-solves dependencies from
  an installed package database, and `cabal-install` currently mis-models
  installed public sublibraries.

This is not primarily a `cardano-wallet` bug, and it is not primarily a
`haskell.nix` bug either, although `haskell.nix` can improve the situation.
The real long-term fix belongs in `cabal-install`.

## Minimal repros

The one-file repro lives in:

- `prototypes/haskell-nix-public-sublib-repro-one-file/flake.nix`

It reproduces the problem in two independent cases:

1. Hackage:
   - dependency: `multi-except:semigroupoid-instances`
2. CHaP:
   - dependency: `io-classes:si-timers`

In both cases:

- `nix build` succeeds
- `nix develop -c cabal build` fails

with the characteristic error:

```text
installed package <pkg-version>/installed-... does not contain library '<sublib>'
```

## What was initially suspected

The first suspicion was that `haskell.nix` was failing to put the public
sublibrary into the shell package database.

That turned out to be false.

The shell package database does contain the right installed units, including the
public sublibrary units.

## What is actually in the package DB

The installed package metadata format already contains the information Cabal
needs:

- source package ID
- source library name
- unit ID
- library visibility
- dependencies by unit ID

See `Distribution.Types.InstalledPackageInfo` in Cabal's
`Cabal-syntax/src/Distribution/Types/InstalledPackageInfo.hs`.

Relevant fields:

- `sourcePackageId`
- `sourceLibName`
- `libVisibility`
- `installedUnitId`
- `depends`

The installed package index also groups installed units by
`(packageName, sourceLibName)`, not just by package name. See
`Distribution.Simple.PackageIndex` in
`Cabal/src/Distribution/Simple/PackageIndex.hs`.

This means the raw GHC package DB format is capable of representing public
sublibraries correctly.

So the DB is not the problem.

## The first `haskell.nix` bug: `ghc` / `ghci`

In the pinned bad `haskell.nix` revision, the dev shell is built by
`shellFor`.

Relevant files:

- `builder/shell-for.nix`
- `builder/make-config-files.nix`
- `builder/comp-builder.nix`

The `haskell.nix` revision inspected for the bad behavior was `ef52c36b`.

What happens:

1. `shellFor` builds a dummy shell component with `doExactConfig = false`.
2. `make-config-files.nix` generates:
   - a `cabal.config`
   - a `ghc-environment` file
3. `shellFor` exports `CABAL_CONFIG`, but not `GHC_ENVIRONMENT`.

Result:

- `ghc` / `ghci` only see the package DB
- they do not see the explicit package exposure from `ghc-environment`
- public sublibraries remain hidden to raw GHC

This was verified directly:

- plain `ghc` in the shell failed with a hidden-package error
- manually exporting `GHC_ENVIRONMENT` to the generated file made `ghc Main.hs`
  work

So there is a real `haskell.nix` shell bug here:

- `shellFor` should export `GHC_ENVIRONMENT`

However, that only fixes `ghc` / `ghci`.

It does **not** fix normal `cabal build`.

## Why `nix build` succeeds

`nix build` is not using the same path as `cabal build` inside `nix develop`.

`nix build` goes through the real `haskell.nix` component builder, which emits
exact dependency flags, including public sublibraries, from
`builder/comp-builder.nix`.

That path uses exact `--dependency` flags such as:

```text
--dependency=multi-except:semigroupoid-instances=<unit-id>
```

So by the time the build runs:

- resolution is already done
- the build is exact
- there is no ambiguous solver step left

This is much closer to invoking `Setup configure --exact-configuration`.

## Why `cabal build` fails in the dev shell

In the dev shell, `cabal build` does **not** receive the exact dependency list.

It sees:

- a compiler
- a package DB
- a `cabal.config`

Then `cabal-install` runs its normal nix-style planner/solver path and tries to
reuse installed packages from that DB.

This is where the real bug appears.

## The crucial distinction: two reuse modes

The confusion comes from the fact that Cabal has two very different kinds of
reuse.

### 1. Reuse of an external installed package DB

This is the failing mode.

In this mode, `cabal build` is given a pre-existing package DB and asked to use
it as the dependency source.

Example mental model:

- the DB contains:
  - `multi-except` main library unit
  - `multi-except:semigroupoid-instances` public sublibrary unit
- user depends on:
  - `multi-except:semigroupoid-instances`

What should happen:

- Cabal should reconstruct:
  - one package `multi-except`
  - with components:
    - main library
    - `semigroupoid-instances`

What currently happens instead:

- Cabal reconstructs installed packages one installed unit at a time
- it effectively turns the DB into:
  - `multi-except` with only a main library
  - a separate munged pseudo-package for the sublibrary

Then the dependency:

```text
multi-except:semigroupoid-instances
```

is checked against installed `multi-except`, and the solver answers:

```text
installed package multi-except does not contain library semigroupoid-instances
```

That is exactly the observed failure.

### 2. Reuse of Cabal's own store after solving from source

This is the working mode.

Here Cabal does **not** plan from an external installed package DB.

Instead:

1. it reads source metadata from Hackage / CHaP / local source packages
2. it solves using the source package description
3. only after the plan is fixed, it checks whether the exact build artifact is
   already present in its store

So the solver already knows the package has the public sublibrary, because it
learned that from the source `.cabal` file, not from the installed package DB.

That is why Cabal can sometimes "not rebuild" and still succeed:

- it solved from source metadata first
- then reused a matching cached build artifact afterward

This is fundamentally different from reusing an arbitrary external package DB.

## Outside-Nix proof

It was important to prove that the bug is not specific to `haskell.nix`.

That proof was done using plain `cabal-install`, outside the `haskell.nix`
shell model.

Tool versions used:

- `ghc 9.10.3`
- `cabal-install 3.16.0.0`

### Experiment structure

Four tiny projects were created:

- `proj-a`
  - depends on `multi-except`
- `proj-b`
  - depends on `multi-except:semigroupoid-instances`
- `proj-c`
  - same dependency as `proj-b`
- `proj-d`
  - same dependency as `proj-b`
  - but with `active-repositories: :none`

### What happened

1. `proj-a` built the main library.
2. `proj-b` built the public sublibrary from source.
3. The Cabal store DB then definitely contained both:
   - the main library unit
   - the public sublibrary unit
4. `proj-c` succeeded without rebuilding the dependency.
   - This is the "solve from source, then reuse store cache" case.
5. `proj-d` was then run with the store DB passed explicitly as an external
   package DB and with source repositories disabled.
6. `proj-d` failed with:

```text
rejecting: multi-except-2.0.0/installed-...
  (does not contain library 'semigroupoid-instances', which is required by proj-d)
```

This is the key proof:

- plain Cabal
- no `haskell.nix`
- external package DB produced by Cabal itself
- DB definitely contains the public sublibrary unit
- `cabal build` still fails when trying to reuse it as an installed package

So the bug is in `cabal-install`'s treatment of pre-existing installed
packages, not in the physical package DB format and not in Nix.

## Why renaming entries in the DB is not a real fix

One idea was to "rename" installed package entries so the solver behaves as if
resolution had already happened.

That does not solve the real problem.

Why:

1. The DB already has the right identity information.
2. `cabal-install` is **already** using a renaming hack internally for
   installed sublibraries.
3. The remaining failure is that the solver model still does not represent
   "one package with multiple public libraries".

The current code:

- recovers a munged package name for installed units
- then hardcodes installed package components as if only the main library
  existed

Relevant file:

- `cabal-install-solver/src/Distribution/Solver/Modular/IndexConversion.hs`

Important spots:

- `convId`
- `convIP`
- `convIPId`

In particular:

- `convIP` currently hardcodes the installed package's components to a singleton
  main library set
- `convIPId` hardcodes installed dependencies to `LMainLibName`

So even after renaming, the solver still lacks the component structure it needs.

There are only two fake alternatives:

1. Keep sublibraries as distinct munged package names.
   - then `pkg:sublib` does not match the installed package the way the solver
     expects
2. Rename them back to the source package name.
   - then the solver sees multiple installed packages with the same name and
     treats them like alternatives, not like one package with multiple public
     library components

So renaming is too weak to encode the semantics Cabal needs.

## Root cause in `cabal-install`

The main bug lives in the installed-package solver conversion path.

Relevant file:

- `cabal-install-solver/src/Distribution/Solver/Modular/IndexConversion.hs`

The source-package path handles public sublibraries correctly.
The installed-package path does not.

Important observations:

1. `convIPI'` iterates over installed units.
2. `convId` munges installed package names to avoid collisions.
3. `convIP` has an explicit `TODO: Handle sub-libraries and visibility.`
4. `convIP` currently builds the `components` map as a singleton:
   - only `ExposedLib LMainLibName`
5. `convIPId` also hardcodes installed dependencies as if they were always
   dependencies on the main library.

This explains the observed rejection exactly.

## Why this is not just a one-line patch

There is also a structural issue in how pre-existing installed packages are
represented later in the pipeline.

Relevant files:

- `cabal-install-solver/src/Distribution/Solver/Types/InstSolverPackage.hs`
- `cabal-install-solver/src/Distribution/Solver/Modular/ConfiguredConversion.hs`
- `cabal-install/src/Distribution/Client/ProjectPlanning.hs`

The current representation assumes a pre-existing solver package is anchored by
one `InstalledPackageInfo`.

That fits "main library only" packages well.
It does not fit "one source package with several public library units" nearly
as well.

So a correct upstream fix likely needs more than just changing `convIP`.

## What already works today

### `cabal act-as-setup`

A heavy workaround exists:

```text
cabal act-as-setup -- configure --exact-configuration --dependency=...
```

This worked for both the Hackage and CHaP repros.

That matters because it proves:

- the low-level `Setup` path can consume public sublibraries correctly
- the failure is specifically in normal `cabal-install` planning / installed
  package reuse

This is useful as a diagnostic proof, but it is not a practical replacement for
normal `cabal build` in `cardano-wallet`.

## Implications for `cardano-wallet`

There are two goals, and they should be treated separately.

### Goal 1: make `nix develop` usable again

Recommended approach:

- stop depending on reuse of a pre-existing Haskell package DB for normal
  `cabal build`
- let `cabal-install` solve from source metadata instead
- keep Nix responsible for:
  - compiler version
  - system libraries
  - tools
  - non-Haskell executables

In practice, this likely means adding a shell variant that does **not** wire
`cabal` to the preinstalled package DB through the `shellFor` model.

This is the robust short-term workaround because it avoids the broken code path
entirely.

Possible variants:

1. Make the default shell a source-solving shell.
   - slower first build
   - most robust
2. Keep the current prebuilt shell for `ghc` / `ghci` / HLS use cases, but add
   a second shell for `cabal build` from source.
3. Add the `GHC_ENVIRONMENT` export fix to improve `ghc` / `ghci`, regardless
   of the shell split.

What is **not** recommended as the main workaround:

- renaming installed package entries
- manually deleting sublibrary entries from the package DB
- trying to fake solver success through DB surgery

Those are brittle and do not address the underlying model mismatch.

### Goal 2: upstream fix in Cabal

The long-term fix belongs in `cabal-install`.

The broad shape of the fix appears to be:

1. When reading installed packages from a package DB, aggregate installed public
   libraries by source package instead of pretending each installed unit is a
   separate package.
2. Preserve the mapping from public library name to installed unit ID.
3. Make installed dependencies component-aware instead of hardcoding main
   library dependencies.
4. Elaborate chosen pre-existing packages back into the right installed units in
   the final plan.
5. Add regression tests for:
   - external DB reuse of `pkg:sublib`
   - source-solve + store-reuse
   - failure without source fallback

## Recommended immediate plan

1. Preserve this note and the one-file repro as the canonical investigation
   record.
2. Implement a `cardano-wallet` shell that avoids the broken external-DB reuse
   path for normal `cabal build`.
3. Separately prepare an upstream `cabal-install` issue and patch series based
   on the solver diagnosis above.
4. Optionally send a smaller `haskell.nix` fix to export `GHC_ENVIRONMENT` for
   better `ghc` / `ghci` behavior.

## Current confidence

High confidence in these statements:

- the dev-shell package DB is not missing the public sublibrary units
- `nix build` and `cabal build` are using materially different paths
- the installed-package reuse bug reproduces outside Nix
- the main long-term fix belongs in `cabal-install`

Medium confidence in this statement:

- the final upstream patch will need modest representation changes beyond
  `IndexConversion.hs`, not just a one-line fix there

## Open follow-up work

1. Identify the exact dependency in `cardano-wallet` that first triggers this
   regression in the real repo build.
2. Prototype a source-solving wallet dev shell.
3. Draft the upstream `cabal-install` regression test from the one-file repro.
4. Decide whether the `haskell.nix` `GHC_ENVIRONMENT` fix should also be sent
   independently.
