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
    branch="''${BUILDKITE_BRANCH:-}"
    tag="''${BUILDKITE_TAG:-}"
    extra_tag=""
    if [[ "$tag" =~ ^v20 ]]; then
      tag="${image.imageTag}"
      extra_tag="${image.backend}"
    elif [[ "$branch" = master ]]; then
      tag="$(echo ${image.imageTag} | sed -e s/${image.version}/''${BUILDKITE_COMMIT:-dev-$branch}/)"
      extra_tag="$(echo ${image.imageTag} | sed -e s/${image.version}/dev-$branch/)"
    else
      echo "Not pushing docker image because this is not a master branch or v20* tag build."
      exit 0
    fi
    echo "Loading ${image}"
    tagged="$fullrepo:$tag"
    docker load -i "${image}"
    if [ "$tagged" != "${image.imageName}:${image.imageTag}" ]; then
      docker tag "${image.imageName}:${image.imageTag}" "$tagged"
    fi
    echo "Pushing $tagged"
    docker push "$tagged"
    if [ -n "$extra_tag" ]; then
      echo "Pushing $fullrepo:$extra_tag"
      docker tag "$tagged" "$fullrepo:$extra_tag"
      docker push "$fullrepo:$extra_tag"

      ${optionalString (image.backend == "byron") ''
      echo "Pushing $fullrepo:latest"
      docker tag "$tagged" "$fullrepo:latest"
      docker push "$fullrepo:latest"
      ''}
    fi
  '') images)
