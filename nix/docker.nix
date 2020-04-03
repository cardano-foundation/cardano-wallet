############################################################################
# Docker image builder
#
# To test it out, use:
#
#   docker load -i $(nix-build -A dockerImage --no-out-link)
#   docker run cardano-wallet-itn
#
############################################################################

{ runtimeShell, writeScriptBin, runCommand, dockerTools

# The main contents of the image.
, cardano-wallet-itn

# Other things to include in the image.
, glibcLocales, iana-etc, cacert
, bashInteractive, coreutils, utillinux, iproute, iputils, curl, socat

# Used to generate the docker image names
, repoName ? "inputoutput/cardano-wallet"
}:

let
  defaultPort = "8090";
  dataDir = "/data";

  startScript = writeScriptBin "start-cardano-wallet-itn" ''
    #!${runtimeShell}
    set -euo pipefail

    # set up data volume
    export XDG_DATA_HOME=/
    mkdir -p ${dataDir}
    ln -s ${dataDir} /cardano-wallet

    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    exec ${cardano-wallet-itn}/bin/cardano-wallet-itn "$@"
  '';

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [
      glibcLocales iana-etc cacert
      bashInteractive coreutils utillinux iproute iputils curl socat
    ];
    # set up /tmp (override with TMPDIR variable)
    extraCommands = "mkdir -m 0777 tmp";
  };

in
  dockerTools.buildImage {
    name = repoName;
    tag = "${cardano-wallet-itn.version}-jormungandr";
    fromImage = baseImage;
    contents = [
      cardano-wallet-itn
      startScript
    ];
    config = {
      EntryPoint = [ "start-cardano-wallet-itn" ];
      ExposedPorts = {
        "${defaultPort}/tcp" = {}; # wallet api
      };
      Volume = [ dataDir ];
    };
  } // { inherit (cardano-wallet-itn) version; }
