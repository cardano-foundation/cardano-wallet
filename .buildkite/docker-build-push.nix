# This script will load nix-built docker images of cardano-wallet
# into the Docker daemon (must be running), and then push to the
# Docker Hub. Credentials for the hub must already be installed with
# "docker login".

{ walletPackages ?  import ../default.nix {}
, pkgs ? walletPackages.pkgs

# Build system's Nixpkgs. We use this so that we have the same docker
# version as the docker daemon.
, hostPkgs ? import <nixpkgs> {}

# Dockerhub repository for image tagging.
, dockerHubRepoName ? null
}:

with hostPkgs;
with hostPkgs.lib;

let
  images = map impureCreated [
    walletPackages.dockerImage
  ];

  # Override Docker image, setting its creation date to the current time rather than the unix epoch.
  impureCreated = image: image.overrideAttrs (oldAttrs: { created = "now"; }) // { inherit (image) version; };

in
  writeScript "docker-build-push" (''
    #!${runtimeShell}

    set -euo pipefail

    export PATH=${lib.makeBinPath [ docker gnused ]}

    ${if dockerHubRepoName == null then ''
    reponame=cardano-wallet
    username="$(docker info | sed '/Username:/!d;s/.* //')"
    fullrepo="$username/$reponame"
    '' else ''
    fullrepo="${dockerHubRepoName}"
    ''}

  '' + concatMapStringsSep "\n" (image: ''
    branch="''${BUILDKITE_BRANCH:-}"
    tag="''${BUILDKITE_TAG:-}"
    if [[ -n "$tag" ]] || [[ "$branch" = "rvl/923/docker" ]]; then
      tag="${image.imageTag}"
    elif [[ "$branch" = master ]]; then
      tag="$(echo ${image.imageTag} | sed -e s/${image.version}/''${BUILDKITE_COMMIT:-dev}/)"
    else
      echo "Not pushing docker image because this is not a master branch or tag build."
      exit 0
    fi
    echo "Loading ${image}"
    tagged="$fullrepo:$tag"
    docker load -i "${image}"
    if [ "$tagged" != "${image.imageName}:${image.imageTag}" ]; then
      docker tag "${image.imageName}:${image.imageTag}" "$tagged"
    fi
    docker push "$tagged"
  '') images)
