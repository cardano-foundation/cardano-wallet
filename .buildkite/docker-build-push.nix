# This script will load nix-built docker images of cardano-wallet
# into the Docker daemon (must be running), and then push to the
# Docker Hub. Credentials for the hub must already be installed with
# "docker login".
#
# There is a little bit of bash logic to replace the default repo and
# tag from the nix-build (../nix/docker.nix).
#
# 1. The repo (default "inputoutput/cardano-wallet") is changed to match
#    the logged in Docker user's credentials. So you can test this with
#    your own Dockerhub account.
#
# 2. The tag (default "VERSION-BACKEND") is changed to reflect the
#    branch which is being built under this Buildkite pipeline.
#
#    - If this is a git tag build (i.e. release) then the docker tag
#      is left as-is.
#    - If this is a master branch build then the docker tag is set to
#      "dev-master-BACKEND".
#    - Anything else is not tagged and not pushed.
#
# 3. After pushing the image to the repo, the "latest" tags are updated.
#
#    - "inputoutput/cardano-wallet:latest" should point to the most
#      recent VERSION-shelley tag build.
#    - "inputoutput/cardano-wallet:shelley" should point to the most
#      recent VERSION-shelley tag build.
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
  images = mapAttrsToList (const impureCreated)
    (filterAttrs (const isDerivation) walletPackages.dockerImage);

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
    echo "Loading ${image}"
    docker load -i "${image}"

    # Apply tagging scheme
    orig_tag="${image.imageName}:${image.imageTag}"
    git_branch="''${BUILDKITE_BRANCH:-}"
    git_tag="''${BUILDKITE_TAG:-}"
    tags=()
    if [[ "$git_tag" =~ ^v20 ]]; then
      tags+=( "${image.imageTag}" )
      tags+=( "${image.backend}" )
      ${optionalString (image.backend == "shelley") ''
      tags+=( "latest" )
      ''}
    elif [[ "$git_branch" = master ]]; then
      tags+=( "$(echo ${image.imageTag} | sed -e s/${image.version}/dev-$git_branch/)" )
    else
      echo 'Not pushing docker image because this is not a master branch or v20* tag build.'
    fi

    for tag in ''${tags[@]}; do
      tagged="$fullrepo:$tag"
      if [ "$tagged" != "$orig_tag" ]; then
        docker tag "$orig_tag" "$tagged"
      fi
      echo "Pushing $tagged"
      docker push "$tagged"
    done
  '') images)
