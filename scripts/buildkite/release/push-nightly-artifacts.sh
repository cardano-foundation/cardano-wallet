#! /bin/bash

set -euox pipefail

base_build=$(buildkite-agent meta-data get base-build)
NEW_GIT_TAG=$(buildkite-agent meta-data get release-version)

unset GITHUB_TOKEN
unset GH_TOKEN
echo "$PUSH_ARTIFACTS_TOKEN" | gh auth login --with-token


main_build=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
    -X GET "https://api.buildkite.com/v2/builds" \
    | jq ".[] | select(.meta_data.\"triggered-by\" == \"$base_build\")" \
    | jq .number)

mkdir -p artifacts
artifact() {
    local artifact_name=$1
    # shellcheck disable=SC2155
    local artifact_value=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
        -X GET "https://api.buildkite.com/v2/organizations/cardano-foundation/pipelines/cardano-wallet/builds/$main_build/artifacts" \
        | jq -r ".[] | select(.filename == \"$artifact_name\") \
        | .download_url")
    curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" -L \
        -o "artifacts/$artifact_name" \
        "$artifact_value"
    gh release upload nightly "artifacts/$artifact_name"
}

artifact "cardano-wallet-$NEW_GIT_TAG-linux64.tar.gz"
artifact "cardano-wallet.exe-$NEW_GIT_TAG-win64.zip"
artifact "cardano-wallet-$NEW_GIT_TAG-macos-silicon.tar.gz"
artifact "cardano-wallet-$NEW_GIT_TAG-macos-intel.tar.gz"
artifact "cardano-wallet-$NEW_GIT_TAG-docker-image.tgz"


# linux_artifact_name="cardano-wallet-$NEW_GIT_TAG-linux64.tar.gz"

# linux_artifact=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
#     -X GET "https://api.buildkite.com/v2/organizations/cardano-foundation/pipelines/cardano-wallet/builds/$main_build/artifacts" \
#     | jq -r ".[] | select(.filename == \"$linux_artifact_name\") \
#     | .download_url")

# curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" -L -o \
#     "$RELEASE_SCRIPTS_DIR/$linux_artifact_name" \
#     "$linux_artifact"

# gh release upload nightly "$RELEASE_SCRIPTS_DIR/$linux_artifact_name"

# windows_artifact_name="cardano-wallet-$NEW_GIT_TAG-win64.zip"

# windows_artifact=$(curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" \
#     -X GET "https://api.buildkite.com/v2/organizations/cardano-foundation/pipelines/cardano-wallet/builds/$main_build/artifacts" \
#     | jq -r ".[] | select(.filename == \"$windows_artifact_name\") \
#     | .download_url")

# curl -H "Authorization: Bearer $BUILDKITE_API_TOKEN" -L -o \
#     "$RELEASE_SCRIPTS_DIR/$windows_artifact_name" \
#     "$windows_artifact"
