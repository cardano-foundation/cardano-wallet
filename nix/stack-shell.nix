# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
{ walletPackages ? import ./.. {}
, pkgs ? walletPackages.pkgs
}:
with pkgs;

haskell.lib.buildStackProject rec {
  name = "cardano-wallet-stack-env";
  ghc = walletPackages.haskellPackages._config.ghc.package;

  buildInputs =
    (with walletPackages; [ jormungandr jormungandr-cli ]) ++
    [ zlib gmp ncurses lzma openssl git systemd.dev nodejs ] ++
    (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ]));

  phases = ["nobuildPhase"];
  nobuildPhase = "echo '${pkgs.lib.concatStringsSep "\n" ([ghc] ++ buildInputs)}' > $out";
}
