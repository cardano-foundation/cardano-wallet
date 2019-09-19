############################################################################
# Packages for jormungandr and jcli.
#
# These use the functions from iohk-nix to build the version that we
# require for cardano-wallet.
#
# To change the version:
# 1. Adjust the "version" and/or "rev" variables below.
# 2. Then change a single digit in *both* sha256 and cargoSha256.
#    (e.g. change a 4 to 5). It's important that you change them to
#    something different than before.
# 3. Run "nix-build -A jormungandr.src" (or let CI do it for you).
#    It will say that the hash is wrong. Update sha256 to the value it got.
# 4. Run "nix-build -A jormungandr". After some time downloading
#    crates, it should say that the vendor hash is wrong.
#    Update cargoSha256 with the value it got.
# 5. Run "nix-build -A jormungandr". It should complete the build.
# 6. Test that "nix-build -A jormungandr-cli" also works.
# 7. If you now run "nix-shell" you should have updated versions of
#    jormungandr and jcli.
#
############################################################################

{ iohkLib ? import ./iohk-common.nix {}
, pkgs ? iohkLib.pkgs
}:

let
  release = rec {
    version = "0.3.3";
    rev = "v${version}";
    sha256 = "1fw3cl2rxnw9mww1b1z96x2iapwbpdgyp4ra19dhvfzmlvaiml5j";
    cargoSha256 = "1ilp9ffaz3njv38mnqics4b5d7wh52mj4rwi71h5c0wzx4ww3zal";
  };

in {
  jormungandr = iohkLib.rust-packages.pkgs.makeJormungandr release;
  jormungandr-cli = iohkLib.rust-packages.pkgs.makeJcli release;
}
