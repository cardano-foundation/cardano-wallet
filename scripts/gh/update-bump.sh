#!/usr/bin/env bash
# Needs yq version 4 and the bump-cli ruby gem

set -euo pipefail

if [ -z "${BUMP_SH_DOC_ID:-}" ] || [ -z "${BUMP_SH_TOKEN:-}" ]; then
  echo "BUMP_SH_DOC_ID or BUMP_SH_TOKEN variables not set" > /dev/stderr
  exit 1
fi

bump_swagger() {
  bump "$1" --doc "$BUMP_SH_DOC_ID" --token "$BUMP_SH_TOKEN" specifications/api/swagger.json
}

yq eval specifications/api/swagger.yaml -o=json > specifications/api/swagger.json

bump_swagger validate
bump_swagger deploy
