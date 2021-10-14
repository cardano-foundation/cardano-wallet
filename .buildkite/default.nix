{ pkgs ? (import ../default.nix {}).legacyPackages.${builtins.currentSystem}.pkgs
  # Include the stack nix-shell in closure of stackRebuild, so that it
  # doesn't get garbage-collected whilst the build is running.
  # https://github.com/commercialhaskell/stack/issues/3479
, stackShell ? import ../nix/stack-shell.nix {}
}:

with pkgs.lib;
with pkgs;

let
  buildTools = [
    gnused gnugrep coreutils git nix gnumake
    gnutar gzip lz4 bzip2 xz
    stack haskell-build-tools.weeder
  ];
  libs = ps: with ps; [turtle safe transformers extra async digest];
  ghc' = haskellPackages.ghcWithPackages libs;

  stackRebuild = runCommand "stack-rebuild" {
    buildInputs = [ ghc' makeWrapper ];
  } ''
    mkdir -p $out/bin
    ghc -Wall -threaded -o $out/bin/rebuild ${./rebuild.hs}
    wrapProgram $out/bin/rebuild \
      --set PATH "${lib.makeBinPath buildTools}" \
      --set NO_GC_STACK_SHELL ${stackShell}
  '';

in
  stackRebuild
