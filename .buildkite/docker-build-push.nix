# This script will load nix-built docker images of cardano-wallet
# into the Docker daemon (must be running), and then push to the
# Docker Hub. Credentials for the hub must already be installed with
# "docker login".
#
# There is a little bit of bash logic to replace the default repo and
# tag from the nix-build (../nix/docker.nix).
#
# 1. So you can test this with your own Dockerhub account, the repo
#    (default "inputoutput/cardano-wallet") is changed to match the
#    currently logged in Docker user's credentials.
#
# 2. The tag (default "VERSION") is changed to reflect the
#    branch which is being built under this Buildkite pipeline.
#
#    - If this is a git tag build (i.e. release) then the docker tag
#      is left as-is.
#    - If this is a master branch build then the docker tag is set to
#      "dev-master".
#    - Anything else is not tagged and not pushed.
#
# 3. After pushing the image to the repo, the "latest" tags are updated.
#
#    - "inputoutput/cardano-wallet:latest" should point to the most
#      recent VERSION tag build (shelley backend).
#

{ defaultNix ? import ../default.nix {}
, pkgs ? defaultNix.legacyPackages.pkgs
, dockerImage ? defaultNix.hydraJobs.linux.musl.dockerImage

# Build system's Nixpkgs. We use this so that we have the same docker
# version as the docker daemon.
, hostPkgs ? import <nixpkgs> {}

# Dockerhub repository for image tagging.
, dockerHubRepoName ? null
}:

with hostPkgs;
with hostPkgs.lib;

let
  images = [ (impureCreated dockerImage) ];

  # Override Docker image, setting its creation date to the current
  # time rather than the unix epoch.
  impureCreated = image:
    image.overrideAttrs (oldAttrs: { created = "now"; })
      // { inherit (image) version backend; };

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
      tags+=( "latest" )
    elif [[ "$git_branch" = master ]]; then
      tags+=( "$(echo ${image.imageTag} | sed -e s/${image.version}/dev-$git_branch/)" )
    else
      echo 'Not pushing docker image because this is not a master branch or v20* tag build.'
    fi

    echo
    echo "Testing that entrypoint works"
    set +e
    docker run --rm "$orig_tag" version
    docker_status="$?"
    if [ "$docker_status" -eq 0 ]; then
      echo "OK"
    elif [ "$docker_status" -eq 125 ]; then
      echo "Docker failed to run ... oh well."
      echo "Continuing..."
    else
      echo "Entrypoint command failed with code $docker_status"
      exit 1
    fi
    set -e
    echo

    for tag in ''${tags[@]}; do
      tagged="$fullrepo:$tag"
      if [ "$tagged" != "$orig_tag" ]; then
        echo "Retagging with $tagged"
        docker tag "$orig_tag" "$tagged"
      fi
      echo "Pushing $tagged"
      docker push "$tagged"
    done
  '') images)
