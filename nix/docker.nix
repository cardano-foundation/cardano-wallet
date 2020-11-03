############################################################################
# Docker image builder
#
# To test it out, use:
#
#   docker load -i $(nix-build -A dockerImage --no-out-link)
#   docker run cardano-wallet
#
############################################################################

{ runtimeShell, writeScriptBin, writeTextFile, runCommand, dockerTools

# The main contents of the image.
, exe
# Short name of the backend, e.g. shelley
, backend

# Other things to include in the image.
, glibcLocales, iana-etc, cacert
, bashInteractive, coreutils, utillinux, iproute, iputils, curl, socat

# Used to generate the docker image names
, repoName ? "inputoutput/cardano-wallet"
}:

let
  defaultPort = "8090";
  dataDir = "/data";

  suffix = if backend == "shelley" then "" else "-" + backend;
  walletExeName = "cardano-wallet" + suffix;
  startScript = writeScriptBin "start-wallet" ''
    #!${runtimeShell}
    set -euo pipefail

    # set up data volume
    export XDG_DATA_HOME=/
    mkdir -p ${dataDir}
    ln -s ${dataDir} /cardano-wallet

    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"
    exec ${exe}/bin/${walletExeName} "$@"
  '';

  # Config file needed for container/host resolution.
  nsswitch-conf = writeTextFile {
    name = "nsswitch.conf";
    text = "hosts: files dns";
    destination = "/etc/nsswitch.conf";
  };

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [
      glibcLocales iana-etc cacert nsswitch-conf
      bashInteractive coreutils utillinux iproute iputils curl socat
    ];
    # set up /tmp (override with TMPDIR variable)
    extraCommands = "mkdir -m 0777 tmp";
  };

in
  dockerTools.buildImage {
    name = repoName;
    tag = "${exe.version}-${backend}";
    fromImage = baseImage;
    contents = [
      exe
      startScript
    ];
    config = {
      EntryPoint = [ "start-wallet" ];
      ExposedPorts = {
        "${defaultPort}/tcp" = {}; # wallet api
      };
      Volume = [ dataDir ];
    };
  } // { inherit (exe) version; inherit backend; }
