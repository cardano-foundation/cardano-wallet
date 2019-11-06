#!/usr/bin/env bash
set -euo pipefail

# This script can be used to re-generate delegation certificates.

# First argument is the index for the account. (e.g. "1")
# Second argument is the folder-name for the stake-pool (e.g. "a")
delegate () {
  jcli certificate new stake-delegation $(cat ../stake_pools/$2/pool_id) $(cat account$1.pub) > stake_delegation$1.cert
  cat stake_delegation$1.cert | jcli certificate sign -k account$1.prv > stake_delegation$1.signedcert
}

delegate 1 a
delegate 2 b
delegate 3 c
