############################################################################
# Packages for jormungandr and jcli.
#
# These use the functions from iohk-nix to build the version that we
# require for cardano-wallet.
#
# To change the version:
# 1. Adjust the "version" and/or "rev" variables below.
# 2. Then invert the first digit in *both* sha256 and
#    cargoSha256. That is, change 0 to 1 and 1 to 0. It's important
#    that you change them to something different than before,
#    otherwise you may get the previous version from your local cache.
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
    version = "0.6.1";
    # Git revision of input-output-hk/jormungandr repo.
    rev = "v${version}";
    # Hash of git repo and all of its submodules.
    sha256 = "1w0xcx1h09wv25qdyybamxxl8sqd2npja12n3vpvz0sgv88c3mix";
    # Hash of all Cargo dependencies.
    cargoSha256 = "0pflam5am760z4pz3j1ga4arsixmay2487sgpqrhrkiaws4nxy57";
  };

  windows = rec {
    # URL and hash of windows binary release
    url = "https://github.com/input-output-hk/jormungandr/releases/download/v${release.version}/jormungandr-v${release.version}-x86_64-pc-windows-msvc.zip";
    sha256 = "0dd2b2r50kcwakn61sv7xvf2s1yl586rlk8zch2bpfw9bjqrs0hl";
  };

  jormungandr-win64 = pkgs.runCommand "jormungandr-win64-${release.version}" {
    nativeBuildInputs = [ pkgs.buildPackages.unzip ];
  } ''
    mkdir -p $out/bin
    cd $out/bin
    unzip ${pkgs.fetchurl windows}
  '';

  nonWindows = pkg: if pkgs.stdenv.hostPlatform.isWindows
    then jormungandr-win64
    else pkg;

in {
  jormungandr = nonWindows (iohkLib.rust-packages.pkgs.makeJormungandr release);
  jormungandr-cli = nonWindows (iohkLib.rust-packages.pkgs.makeJcli release);

  inherit jormungandr-win64;
}
