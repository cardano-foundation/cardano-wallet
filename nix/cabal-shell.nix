# This shell file provides a compiler, build tools and system
# libraries, and lets Cabal build all of the Haskell package
# dependencies.
#
# The default shell (../shell.nix) uses the Haskell.nix shellFor to
# also provide Haskell package dependencies in the shell environment.

{ walletPackages ? import ../default.nix {}
, pkgs ? walletPackages.private.pkgs
# optional string argument to override compiler, e.g.
#   nix-shell nix/cabal-shell.nix --argstr ghcVersion ghc8105
, ghcVersion ? null
# Enable building the cabal-cache util - only needed under CI
, withCabalCache ? false
}:

with pkgs;

mkShell rec {
  name = "cardano-wallet-cabal-env";
  meta.platforms = lib.platforms.unix;

  ghc = if (ghcVersion == null)
    then walletPackages.private.project.pkg-set.config.ghc.package
    else haskell-nix.compiler.${ghcVersion};

  tools = [
    ghc
    haskell-build-tools.cabal-install
    nix
    pkgconfig
  ]
  ++ lib.optional (!stdenv.isDarwin) git
  ++ (with walletPackages; [
    cardano-node
    cardano-cli
  ])
  ++ lib.optional withCabalCache haskell-build-tools.cabal-cache;

  libs = [
    zlib
    gmp
    ncurses
    lzma
    openssl
    libsodium-vrf
    pcre
  ]
  ++ lib.optional (stdenv.hostPlatform.libc == "glibc") glibcLocales
  ++ lib.optional stdenv.isLinux systemd.dev
  ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa CoreServices libcxx libiconv
  ]);

  buildInputs = tools ++ libs;

  # allow building the shell so that it can be cached in hydra
  phases = ["nobuildPhase"];
  nobuildPhase = "echo '${lib.concatStringsSep "\n" buildInputs}' > $out";
  preferLocalBuild = true;

  # Ensure that libz.so and other libraries are available to TH splices.
  LD_LIBRARY_PATH = lib.makeLibraryPath libs;

  # Force a UTF-8 locale because many Haskell programs and tests assume this.
  LANG = "en_US.UTF-8";
}
