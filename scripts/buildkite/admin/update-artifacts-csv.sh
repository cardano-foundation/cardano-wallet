#! /usr/bin/env -S nix shell .#artifacts-csv  -c bash
# shellcheck shell=bash

set -euox pipefail

mkdir -p ./artifacts-history

artifacts-csv --output artifacts-history/artifacts.csv --until 7000
