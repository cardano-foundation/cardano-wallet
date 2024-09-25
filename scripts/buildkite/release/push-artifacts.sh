#! /usr/bin/env bash

set -euox pipefail

base_build=$(buildkite-agent meta-data get base-build)
NEW_GIT_TAG=$(buildkite-agent meta-data get release-version)

if [ "$RELEASE" == "false" ]; then
    TAG=nightly
else
    TAG=$NEW_GIT_TAG
fi

main_build=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
    -X GET "https://api.buildkite.com/v2/builds" \
    | jq ".[] | select(.meta_data.\"triggered-by\" == \"$base_build\")" \
    | jq .number)

mkdir -p artifacts

artifact() {
    local artifact_name=$1
    # shellcheck disable=SC2155
    local artifact_value=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
        -X GET "https://api.buildkite.com/v2/organizations/cardano-foundation/pipelines/cardano-wallet/builds/$main_build/artifacts?per_page=100" \
        | jq -r ".[] | select(.filename == \"$artifact_name\") \
        | .download_url")
    curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" -L \
        -o "artifacts/$artifact_name" \
        "$artifact_value"
    gh release upload "$TAG" "artifacts/$artifact_name"
}

artifact "cardano-wallet-$NEW_GIT_TAG-linux64.tar.gz"
artifact "cardano-wallet.exe-$NEW_GIT_TAG-win64.zip"
artifact "cardano-wallet-$NEW_GIT_TAG-macos-silicon.tar.gz"
artifact "cardano-wallet-$NEW_GIT_TAG-macos-intel.tar.gz"
artifact "cardano-wallet-$NEW_GIT_TAG-docker-image.tgz"
