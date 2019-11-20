############################################################################
# Docker image builder
#
# To test it out, use:
#
#   docker load -i $(nix-build -A dockerImage --no-out-link)
#   docker run cardano-wallet-jormungandr
#
############################################################################

{ runtimeShell, writeScriptBin, runCommand, dockerTools

# The main contents of the image.
, cardano-wallet-jormungandr

# Other things to include in the image.
, glibcLocales, iana-etc, bashInteractive, coreutils, utillinux, iproute, iputils, curl, socat

# Used to generate the docker image names
, repoName ? "inputoutput/cardano-wallet"
}:

let
  defaultPort = "8090";

  startScript = writeScriptBin "start-cardano-wallet-jormungandr" ''
    #!${runtimeShell}
    set -euo pipefail
    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    exec ${cardano-wallet-jormungandr}/bin/cardano-wallet-jormungandr "$@"
  '';

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [ iana-etc bashInteractive coreutils utillinux iproute iputils curl socat ];
  };

in
  dockerTools.buildImage {
    name = repoName;
    tag = "${cardano-wallet-jormungandr.version}-jormungandr";
    fromImage = baseImage;
    contents = [
      cardano-wallet-jormungandr
      startScript
    ];
    config = {
      EntryPoint = [ "start-cardano-wallet-jormungandr" ];
      # Cmd = [ "--port" defaultPort ];
      ExposedPorts = {
        "${defaultPort}/tcp" = {}; # wallet api
      };
    };
  } // { inherit (cardano-wallet-jormungandr) version; }
