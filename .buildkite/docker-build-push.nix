# This script will load nix-built docker images of cardano-wallet
# into the Docker daemon (must be running), and then push to the
# Docker Hub. Credentials for the hub must already be installed with
# "docker login".
#
# There is a little bit of bash logic to replace the default repo and
# tag from the nix-build (../nix/docker.nix).
#
# 1. The repo (default "inputoutput/cardano-wallet") is changed to match
#    the logged in Docker user's credentials.
#
# 2. The tag (default "VERSION-jormungandr") is changed to reflect the
#    branch which is being built under this Buildkite pipeline.
#
#    - If this is a git tag build (i.e. release) then the docker tag
#      is left as-is.
#    - If this is a master branch build, then VERSION is replaced with
#      the git revision.
#    - Anything else is not tagged and not pushed.
#
# 3. After pushing the image to the repo, the "latest" tag is updated.
#
#    - "inputoutput/cardano-wallet:latest" should point to the most
#      recent VERSION-byron tag build.
#    - "inputoutput/cardano-wallet:byron" should point to the most
#      recent VERSION-byron tag build.
#    - "inputoutput/cardano-wallet:jormungandr" should point to the most
#      recent VERSION-jormungandr tag build.
#    - "inputoutput/cardano-wallet:dev-master-jormungandr" should
#      point to the most recent master branch build.
#

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
    walletPackages.dockerImage.jormungandr
    walletPackages.dockerImage.byron
  ];

  # Override Docker image, setting its creation date to the current time rather than the unix epoch.
  impureCreated = image: image.overrideAttrs (oldAttrs: { created = "now"; }) // { inherit (image) version backend; };

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
    gitrev=${pkgs.iohkNix.commitIdFromGitRepoOrZero ../.git}
    branch="''${BUILDKITE_BRANCH:-}"

    echo "Loading ${image}"
    docker load -i "${image}"

    # An image is tagged for each commit
    echo "Pushing $fullrepo:$gitrev-${image.backend}"
    docker tag "${image.imageName}:${image.imageTag}" "$fullrepo:$gitrev-${image.backend}"
    docker push "$fullrepo:$gitrev-${image.backend}"

    # If branch is master, tag with "dev-master-${image.backend}"
    if [[ "$branch" == master ]]; then
      echo "Pushing $fullrepo:dev-master-${image.backend}"
      docker tag "$fullrepo:$gitrev-${image.backend}" "$fullrepo:dev-master-${image.backend}"
      docker push "$fullrepo:dev-master-${image.backend}"
    fi

    # If a release event:
    event="''${GITHUB_EVENT_NAME:-}"
    if [[ "$event" == release ]]; then
      # Tag with the image tag and image backend, e.g. "2020.01.01-byron"
      echo "Tagging with a version number: $fullrepo:${image.imageTag}"
      docker tag "$fullrepo:$gitrev-${image.backend}" "$fullrepo:${image.imageTag}"
      echo "Pushing $fullrepo:${image.imageTag}"
      docker push "$fullrepo:${image.imageTag}"

      # Tag with image backend, e.g. "byron" or "jormungandr"
      echo "Tagging with: $fullrepo:${image.backend}"
      docker tag "$fullrepo:${image.imageTag}" "$fullrepo:${image.backend}"
      echo "Pushing $fullrepo:${image.backend}"
      docker push "$fullrepo:${image.backend}"

      # Apply "latest" tag if "byron" backend
      ${(if image.backend == "byron" then ''
        echo "Tagging as latest"
        docker tag "$fullrepo:${image.imageTag}" "$fullrepo:latest"
        echo "Pushing $fullrepo:latest"
        docker push "$fullrepo:latest"
      '' else "")}
    fi
  '') images)
