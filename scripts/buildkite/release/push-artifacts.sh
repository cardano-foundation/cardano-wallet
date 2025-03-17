#! /usr/bin/env bash

set -euox pipefail

TRIGGERED_BY=$(buildkite-agent meta-data get base-build)
NEW_GIT_TAG=$(buildkite-agent meta-data get release-version)
TEST_RC=$(buildkite-agent meta-data get test-rc)

if [ "$RELEASE" == "false" ]; then
    if [ "$TEST_RC" == "TRUE" ]; then
        TAG="test"
    else
        TAG="nightly"
    fi
else
    TAG=$NEW_GIT_TAG
fi

select_last_build="last(.[] | select(.meta_data.\"triggered-by\" == \"$TRIGGERED_BY\") | .number)"

main_build=$(curl -s -H "Authorization: Bearer $BUILDKITE_API_TOKEN"     \
    -X GET "https://api.buildkite.com/v2/builds"     \
    | jq  "$select_last_build"
    )

mkdir -p artifacts

artifact() {
    local artifact_name=$1
    local new_artifact_name=$2
    # shellcheck disable=SC2155
    local artifact_value=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
        -X GET "https://api.buildkite.com/v2/organizations/cardano-foundation/pipelines/cardano-wallet/builds/$main_build/artifacts?per_page=100" \
        | jq -r " [.[] | select(.filename == \"$artifact_name\")][0] \
        | .download_url")
    curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" -L \
        -o "artifacts/$artifact_name" \
        "$artifact_value"
    if [ "$artifact_name" != "$new_artifact_name" ]; then
        mv "artifacts/$artifact_name" "artifacts/$new_artifact_name"
    fi
    gh release upload "$TAG" "artifacts/$new_artifact_name"
}

artifact "cardano-wallet-$NEW_GIT_TAG-linux64.tar.gz" "cardano-wallet-$TAG-linux64.tar.gz"
artifact "cardano-wallet.exe-$NEW_GIT_TAG-win64.zip" "cardano-wallet.exe-$TAG-win64.zip"
artifact "cardano-wallet-$NEW_GIT_TAG-macos-silicon.tar.gz" "cardano-wallet-$TAG-macos-silicon.tar.gz"
# artifact "cardano-wallet-$NEW_GIT_TAG-macos-intel.tar.gz" "cardano-wallet-$TAG-macos-intel.tar.gz"
artifact "cardano-wallet-$NEW_GIT_TAG-docker-image.tgz" "cardano-wallet-$TAG-docker-image.tgz"
