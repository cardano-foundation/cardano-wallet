############################################################################
# Docker image builder
#
# To test it out, use:
#
#   docker load -i $(nix build --json .#dockerImage | jq -r '.[0].outputs.out')
#   docker run cardano-wallet
#
############################################################################

{ lib, stdenv
, runtimeShell, writeScriptBin, writeTextFile, dockerTools

# The main contents of the image: cardano-wallet executables
, exes
# Executables to include in the image as a base layer: node and utilities
, base ? []
# Other things to include in the image.
, iana-etc, cacert, bashInteractive, coreutils
, glibcLocales ? null

# Used to generate the docker image names
, repoName ? "inputoutput/cardano-wallet"
}:


let
  version = (lib.head exes).version;

  defaultPort = "8090";
  dataDir = "/data";

  startScript = writeScriptBin "start-cardano-wallet" ''
    #!${runtimeShell}
    set -euo pipefail

    # set up data volume
    export XDG_DATA_HOME=/
    mkdir -p ${dataDir}
    ln -s ${dataDir} /cardano-wallet

    ${lib.optionalString haveGlibcLocales ''
      export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    ''}

    exec /bin/cardano-wallet "$@"
  '';

  haveGlibcLocales = glibcLocales != null &&
    stdenv.hostPlatform.libc == "glibc";

  # Config file needed for container/host resolution.
  nsswitch-conf = writeTextFile {
    name = "nsswitch.conf";
    text = "hosts: files dns";
    destination = "/etc/nsswitch.conf";
  };

  # System environment layer, which isn't going to change much between
  # versions.
  envImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [
      iana-etc cacert nsswitch-conf
      bashInteractive coreutils
    ] ++ lib.optional haveGlibcLocales glibcLocales;

    # set up /tmp (override with TMPDIR variable)
    extraCommands = "mkdir -m 0777 tmp";
  };

  # Layer containing cardano-node backend and Adrestia toolbelt.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-base";
    contents = base;
    fromImage = envImage;
  };

in
  dockerTools.buildImage {
    name = repoName;
    tag = version;
    fromImage = baseImage;
    contents = exes ++ [ startScript ];
    config = {
      EntryPoint = [ "start-cardano-wallet" ];
      ExposedPorts = {
        "${defaultPort}/tcp" = {}; # wallet api
      };
      Volume = [ dataDir ];
    };
  } // { inherit version; }
