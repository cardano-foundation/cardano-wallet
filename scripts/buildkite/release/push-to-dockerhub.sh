#!/usr/bin/env bash

set -euox pipefail

TRIGGERED_BY=$(buildkite-agent meta-data get base-build)
NEW_GIT_TAG=$(buildkite-agent meta-data get release-version)
TEST_RC=$(buildkite-agent meta-data get test-rc)
CABAL_VERSION=$(buildkite-agent meta-data get release-cabal-version)

if [ "$RELEASE" == "false" ]; then
    if [ "$TEST_RC" == "TRUE" ]; then
        TAG="test"
    else
        TAG="nightly"
    fi
else
    TAG=$NEW_GIT_TAG
fi

main_build=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
    -X GET "https://api.buildkite.com/v2/builds" \
    | jq ".[] | select(.meta_data.\"triggered-by\" == \"$TRIGGERED_BY\")" \
    | jq .number)

mkdir -p artifacts

repo="cardanofoundation/cardano-wallet"

artifact() {
    local artifact_name=$1
    # shellcheck disable=SC2155
    local artifact_value=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
        -X GET "https://api.buildkite.com/v2/organizations/cardano-foundation/pipelines/cardano-wallet/builds/$main_build/artifacts?per_page=100" \
        | jq -r " [.[] | select(.filename == \"$artifact_name\")][0] \
        | .download_url")
    curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" -L \
        -o "artifacts/$artifact_name" \
        "$artifact_value"
    docker login -u cfhal -p "$DOCKER_HUB_TOKEN"
    docker load -i "artifacts/$artifact_name"
    local image_name="$repo:$TAG"
    if [ "$RELEASE" == "false" ]; then
        local loaded_image_name="$repo:$CABAL_VERSION"
        docker tag "$loaded_image_name" "$image_name"
        docker push "$image_name"
    else
        local latest_image_name="$repo:latest"
        docker push "$image_name"
        docker tag "$image_name" "$latest_image_name"
        docker push "$latest_image_name"
    fi
}

artifact "cardano-wallet-$NEW_GIT_TAG-docker-image.tgz"