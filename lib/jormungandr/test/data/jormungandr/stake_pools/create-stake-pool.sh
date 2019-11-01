#!/usr/bin/env bash
set -euo pipefail

# This script (re)generates the required keys and certificates to setup a
# stake-pool.
#
# Generated keys and certificates are checked in to git, and can manually be
# inlined in genesis.yaml.
#
# The files are stored in a folder such that multiple stake-pools can be setup
# in a convenient way.
#
# Usage:
# Run in the stake_pools directory, where owner.prv exists.
#
# To create a stake pool in a folder "a":
# .create-stake-pool.sh a
#
# For context, see also:
# https://input-output-hk.github.io/jormungandr/stake_pool/registering_stake_pool.html

echo "creating certificates for a new stake pool in the folder $1"
mkdir $1
cd $1

jcli key generate --type=Curve25519_2HashDH > stake_pool_vrf.prv
cat stake_pool_vrf.prv | jcli key to-public > stake_pool_vrf.pub
jcli key generate --type=SumEd25519_12 > stake_pool_kes.prv
cat stake_pool_kes.prv | jcli key to-public > stake_pool_kes.pub

jcli certificate new stake-pool-registration \
    --kes-key $(cat stake_pool_kes.pub) \
    --vrf-key $(cat stake_pool_vrf.pub) \
    --serial 1010101010 \
    --start-validity 0 \
    --owner $(cat ../owner.pub) \
    --management-threshold 1 > stake_pool.cert

cat stake_pool.cert | jcli certificate sign ../owner.prv | tee stake_pool.cert > stake_pool_signed.cert

cat stake_pool.cert | jcli certificate get-stake-pool-id | tee stake_pool.id > pool_id
