# This shell file provides a compiler, build tools and system
# libraries, and lets Cabal build all of the Haskell package
# dependencies.
#
# The default shell (../shell.nix) uses the Haskell.nix shellFor to
# also provide Haskell package dependencies in the shell environment.

{ haskellProject
# Enable building the cabal-cache util - only needed under CI
, withCabalCache
# optional string argument to override compiler, e.g.
#   nix-shell nix/cabal-shell.nix --argstr ghcVersion ghc8105
, ghcVersion
}:

with haskellProject.pkgs;

mkShell rec {
  name = "cardano-wallet-cabal-env";
  meta.platforms = lib.platforms.unix;

  ghc = if (ghcVersion == null)
    then haskellProject.pkg-set.config.ghc.package
    else haskell-nix.compiler.${ghcVersion};

  tools = [
    ghc
    haskell-build-tools.cabal-install
    nixWrapped
    pkgconfig
    gnutar
  ]
  ++ lib.optional (!stdenv.isDarwin) git
  ++ (with haskellProject.hsPkgs; [
    cardano-node.components.exes.cardano-node
    cardano-cli.components.exes.cardano-cli
  ])
  ++ lib.optional withCabalCache haskell-build-tools.cabal-cache;

  libs = [
    xz
    zlib
    bzip2
    lzma
    gmp
    ncurses
    openssl
    libsodium-vrf
    pcre
  ]
  ++ lib.optional (stdenv.hostPlatform.libc == "glibc") glibcLocales
  ++ lib.optional stdenv.isLinux systemd.dev
  ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa CoreServices libcxx libiconv
  ]);

  nativeBuildInputs = tools ++ libs;

  # allow building the shell so that it can be cached in hydra
  phases = ["nobuildPhase"];
  nobuildPhase = "echo '${lib.concatStringsSep "\n" nativeBuildInputs}' > $out";
  preferLocalBuild = true;

  # Ensure that libz.so and other libraries are available to TH splices.
  LD_LIBRARY_PATH = lib.makeLibraryPath libs;

  # Force a UTF-8 locale because many Haskell programs and tests assume this.
  LANG = "en_US.UTF-8";

  # Provide SSL certificates for git, in case we are running in a pure nix-shell.
  GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
}
