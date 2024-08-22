#! /bin/bash

set -euox pipefail

NEW_GIT_TAG=$(buildkite-agent meta-data get release-version)

unset GITHUB_TOKEN
unset GH_TOKEN
echo "$PUSH_ARTIFACTS_TOKEN" | gh auth login --with-token

gh release delete "$TAG" --yes || true
gh release create \
    -d \
    -F "$RELEASE_SCRIPTS_DIR/release-template.md" \
    -t "Nightly $NEW_GIT_TAG" \
    "nightly"
