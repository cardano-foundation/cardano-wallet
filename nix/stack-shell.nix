# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
{ walletPackages ? import ../default.nix {}
, pkgs ? walletPackages.pkgs
}:
with pkgs;

mkShell rec {
  name = "cardano-wallet-stack-env";

  buildInputs =
    (with walletPackages; [
      haskellPackages._config.ghc.package
      jormungandr
      jormungandr-cli
      cardano-node
      cardano-cli
    ]) ++ [
      zlib
      gmp
      ncurses
      lzma
      openssl
      libsodium
      pkgconfig
    ] ++ lib.optional (stdenv.hostPlatform.libc == "glibc") glibcLocales ++
    (lib.optionals (!stdenv.isDarwin) [ git systemd.dev ]) ++
    (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ]));

  # allow building the shell so that it can be cached in hydra
  phases = ["nobuildPhase"];
  nobuildPhase = "echo '${pkgs.lib.concatStringsSep "\n" buildInputs}' > $out";
  preferLocalBuild = true;

  meta.platforms = lib.platforms.unix;

  # Build environment setup copied from
  # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
  LANG = "en_US.UTF-8";
}
