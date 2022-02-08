{ pkgs ? import <nixpkgs> {}
, extraArgs ? []
, stackExtraArgs ? extraArgs
# optional string argument to override compiler, e.g.
#   nix-shell nix/stack-shell.nix --argstr ghcVersion ghc8105
, ghcVersion ? "ghc8107"
, ... }@args:

with pkgs;

mkShell rec {
  name = "cardano-wallet-stack-env";
  meta.platforms = lib.platforms.unix;

  ghc = haskell.compiler.${ghcVersion};

  tools = [
    ghc
    cabal-install
    stack
    pkgconfig
    gnutar
  ]
  ++ lib.optional (!stdenv.isDarwin) git;

  libs = [
    xz
    zlib
    bzip2
    lzma
    gmp
    ncurses
    openssl
    # libsodium-vrf
    pcre
  ]
  ++ lib.optional (stdenv.hostPlatform.libc == "glibc") glibcLocales
  ++ lib.optional stdenv.isLinux systemd.dev
  ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
    libcxx
    libiconv
  ]);

  nativeBuildInputs = tools ++ libs;

  # allow building the shell so that it can be cached in hydra
  phases = [ "nobuildPhase" ];
  nobuildPhase = "echo '${lib.concatStringsSep "\n" nativeBuildInputs}' > $out";
  preferLocalBuild = true;

  # Ensure that libz.so and other libraries are available to TH splices.
  LD_LIBRARY_PATH = lib.makeLibraryPath libs;

  # Force a UTF-8 locale because many Haskell programs and tests assume this.
  LANG = "en_US.UTF-8";

  # Provide SSL certificates for git, in case we are running in a pure nix-shell.
  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  # Build environment setup copied from
  # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
  STACK_PLATFORM_VARIANT = "nix";
  STACK_IN_NIX_SHELL = 1;
  STACK_IN_NIX_EXTRA_ARGS = stackExtraArgs;
}
