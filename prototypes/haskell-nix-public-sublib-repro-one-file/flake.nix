# Repro commands:
#   nix build --no-write-lock-file \
#     --option extra-substituters https://cache.iog.io \
#     --option extra-trusted-public-keys \
#     'hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' \
#     .#hackage
#
#   src=$(nix build --no-write-lock-file \
#     --option extra-substituters https://cache.iog.io \
#     --option extra-trusted-public-keys \
#     'hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' \
#     .#hackage-src --print-out-paths --no-link) && \
#   tmp=$(mktemp -d) && cp -R "$src"/. "$tmp" && chmod -R u+w "$tmp" && \
#   nix develop --no-write-lock-file \
#     --option extra-substituters https://cache.iog.io \
#     --option extra-trusted-public-keys \
#     'hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' \
#     .#hackage -c bash -lc 'cd "$1" && cabal build all' bash "$tmp"
#   Expected failure: installed multi-except does not contain
#   library 'semigroupoid-instances'.
#
#   nix build --no-write-lock-file \
#     --option extra-substituters https://cache.iog.io \
#     --option extra-trusted-public-keys \
#     'hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' \
#     .#chap
#
#   src=$(nix build --no-write-lock-file \
#     --option extra-substituters https://cache.iog.io \
#     --option extra-trusted-public-keys \
#     'hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' \
#     .#chap-src --print-out-paths --no-link) && \
#   tmp=$(mktemp -d) && cp -R "$src"/. "$tmp" && chmod -R u+w "$tmp" && \
#   nix develop --no-write-lock-file \
#     --option extra-substituters https://cache.iog.io \
#     --option extra-trusted-public-keys \
#     'hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=' \
#     .#chap -c bash -lc 'cd "$1" && cabal build all' bash "$tmp"
#   Expected failure: installed io-classes does not contain library
#   'si-timers'. Cabal may then also complain about QuickCheck.
{
  description = "One-file haskell.nix repro for public sublibraries from Hackage and CHaP";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix/ef52c36b9835c77a255befe2a20075ba71e3bfab";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/5aed5285a952e0b949eb3ba02c12fa4fcfef535f";
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages/d8156d61840f90f0721c396f0598652f7aaf402a";
      flake = false;
    };
  };

  outputs =
    {
      self,
      CHaP,
      flake-utils,
      haskellNix,
      nixpkgs,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };

        mkSource =
          {
            name,
            cabalProject,
            cabalFileName,
            cabalFile,
            mainHs,
          }:
          pkgs.runCommand name { } ''
            mkdir -p "$out/app"

            cat > "$out/cabal.project" <<'EOF'
            ${cabalProject}
            EOF

            cat > "$out/${cabalFileName}" <<'EOF'
            ${cabalFile}
            EOF

            cat > "$out/app/Main.hs" <<'EOF'
            ${mainHs}
            EOF
          '';

        hackageSrc = mkSource {
          name = "hackage-sublib-repro-src";
          cabalProject = ''
            packages: .

            index-state: 2024-01-03T15:04:34Z

            constraints:
              multi-except ==2.0.0
          '';
          cabalFileName = "hackage-sublib-repro.cabal";
          cabalFile = ''
            cabal-version: 3.0
            name: hackage-sublib-repro
            version: 0.1.0.0
            build-type: Simple

            executable hackage-sublib-repro
              main-is: Main.hs
              hs-source-dirs: app
              default-language: Haskell2010
              build-depends:
                  base >=4.14 && <5
                , multi-except:semigroupoid-instances
          '';
          mainHs = ''
            module Main where

            import Control.Applicative.MultiExcept.Alt ()

            main :: IO ()
            main = putStrLn "Imported multi-except:semigroupoid-instances"
          '';
        };

        chapSrc = mkSource {
          name = "chap-sublib-repro-src";
          cabalProject = ''
            repository cardano-haskell-packages
              url: https://chap.intersectmbo.org/
              secure: True
              root-keys:
                3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
                443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
                a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
                bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
                c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
                d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee

            active-repositories: hackage.haskell.org, cardano-haskell-packages

            index-state:
              , hackage.haskell.org 2025-11-05T00:21:20Z
              , cardano-haskell-packages 2026-04-15T07:51:03Z

            packages: .

            constraints:
              io-classes ==1.10.1.0
          '';
          cabalFileName = "chap-sublib-repro.cabal";
          cabalFile = ''
            cabal-version: 3.0
            name: chap-sublib-repro
            version: 0.1.0.0
            build-type: Simple

            executable chap-sublib-repro
              main-is: Main.hs
              hs-source-dirs: app
              default-language: Haskell2010
              build-depends:
                  base >=4.16 && <5
                , io-classes:si-timers
          '';
          mainHs = ''
            module Main where

            import Control.Monad.Class.MonadTimer.SI ()

            main :: IO ()
            main = putStrLn "Imported io-classes:si-timers"
          '';
        };

        mkProject =
          {
            name,
            src,
            extraArgs ? { },
          }:
          pkgs.haskell-nix.cabalProject' (
            {
              inherit name src;
              compiler-nix-name = "ghc966";
              shell = {
                exactDeps = true;
                nativeBuildInputs = [ pkgs.cabal-install ];
                withHoogle = false;
              };
            }
            // extraArgs
          );

        hackageProject = mkProject {
          name = "hackage-public-sublib-repro";
          src = hackageSrc;
        };

        chapProject = mkProject {
          name = "chap-public-sublib-repro";
          src = chapSrc;
          extraArgs = {
            inputMap = {
              "https://chap.intersectmbo.org/" = CHaP;
            };
          };
        };
      in
      {
        packages = {
          hackage = hackageProject.hsPkgs."hackage-sublib-repro".components.exes."hackage-sublib-repro";
          hackage-src = hackageSrc;
          chap = chapProject.hsPkgs."chap-sublib-repro".components.exes."chap-sublib-repro";
          chap-src = chapSrc;
          default = hackageProject.hsPkgs."hackage-sublib-repro".components.exes."hackage-sublib-repro";
        };

        devShells = {
          hackage = hackageProject.shell;
          chap = chapProject.shell;
          default = hackageProject.shell;
        };
      }
    );
}
