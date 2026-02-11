#!/usr/bin/env bash

set -euox pipefail


GOPATH=$(mktemp -d)

export GOPATH

# go install is going to write unremovable stuff to the $GOPATH/bin directory
go install github.com/oasdiff/oasdiff@latest

swagger_tmp=$(mktemp -d)
swagger_file="specifications/api/swagger.yaml"
last_release_tag=$(curl -s https://api.github.com/repos/cardano-foundation/cardano-wallet/releases | jq -r '.[0].tag_name')

git fetch --tags --force

git show "$last_release_tag:$swagger_file" > "$swagger_tmp/last-release-swagger.yaml"

mkdir -p artifacts
"$GOPATH"/bin/oasdiff diff \
    "$swagger_tmp/last-release-swagger.yaml" \
    "$swagger_file" \
     -f markup \
    > "artifacts/api-diff.md"

# free the bin directory for buildkite to clean up
chmod -R +w "$GOPATH"
rm -rf "$GOPATH"
rm -rf "$swagger_tmp"
