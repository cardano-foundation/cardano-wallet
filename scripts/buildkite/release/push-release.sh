#! /usr/bin/env bash

set -euox pipefail

NEW_GIT_TAG=$(buildkite-agent meta-data get release-version)
TEST_RC=$(buildkite-agent meta-data get test-rc)

if [ "$RELEASE" == "false" ]; then
    if [ "$TEST_RC" == "FALSE" ]; then
        TAG=nightly
        title="Nightly $NEW_GIT_TAG"
    else
        TAG="test"
        title="Test $NEW_GIT_TAG"
    fi

else
    TAG=$NEW_GIT_TAG
    title="Release $TAG"
fi

buildkite-agent artifact download artifacts/changes.md .
# shellcheck disable=SC2155
export CHANGES=$(cat artifacts/changes.md)

# shellcheck disable=SC2155
export NODE_TAG=$(buildkite-agent meta-data get node-tag)

buildkite-agent artifact download artifacts/api-diff.md .
# shellcheck disable=SC2155
export API_CHANGES=$(cat artifacts/api-diff.md)

# shellcheck disable=SC2034
export DOCKER_SHA=xxx


# shellcheck disable=SC2016
envsubst \
    < "$RELEASE_SCRIPTS_DIR/release-template.md" \
    > "$RELEASE_SCRIPTS_DIR/release-template-final.md"

if [ "$RELEASE" == "false" ]; then
    gh release delete "$TAG" --yes || true
fi

gh release create \
    -d \
    -F "$RELEASE_SCRIPTS_DIR/release-template-final.md" \
    -t "$title" \
    "$TAG"
