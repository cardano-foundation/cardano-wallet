# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
{ system ? builtins.currentSystem
, config ? {}
, walletPackages ? import ./.. { inherit config system; }
, pkgs ? walletPackages.pkgs
}:
with pkgs;

haskell.lib.buildStackProject {
  name = "cardano-wallet-stack-env";
  ghc = haskell.compiler.ghc863;

  buildInputs = [
    zlib gmp ncurses lzma openssl git
    walletPackages.cardano-http-bridge
  ] ++ (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ]));

  phases = ["nobuildPhase"];
  nobuildPhase = "mkdir -p $out";
}
