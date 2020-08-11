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
# To only regenerate the certificate only (using existing keys), run:
# .create-stake-pool.sh a --update
# For context, see also:
# https://input-output-hk.github.io/jormungandr/stake_pool/registering_stake_pool.html

echo "Using $(jcli --version)"

if [[ $* == *--update* ]]; then
  echo "regenerating stake pool certificates the folder $1 using existing keys"
  cd $1
else
  echo "creating certificates for a new stake pool in the folder $1"
  mkdir $1
  cd $1
  jcli key generate --type=Curve25519_2HashDH > stake_pool_vrf.prv
  cat stake_pool_vrf.prv | jcli key to-public > stake_pool_vrf.pub
  jcli key generate --type=SumEd25519_12 > stake_pool_kes.prv
  cat stake_pool_kes.prv | jcli key to-public > stake_pool_kes.pub
fi

jcli certificate new stake-pool-registration \
    --kes-key $(cat stake_pool_kes.pub) \
    --vrf-key $(cat stake_pool_vrf.pub) \
    --start-validity 0 \
    --owner $(cat ../owner.pub) \
    --tax-ratio "0/1" \
    --tax-fixed 0 \
    --management-threshold 1 > stake_pool.cert

cat stake_pool.cert | jcli certificate sign -k ../owner.prv > stake_pool.cert.signed

cat stake_pool.cert | jcli certificate get-stake-pool-id > stake_pool.id

# Transaction to register (without fees)
jcli transaction new \
    | jcli transaction add-certificate $(cat stake_pool.cert) \
    | jcli transaction finalize \
    | jcli transaction seal \
    | jcli transaction auth -k ../owner.prv \
    > registration_no_fees.tx

jcli transaction fragment-id < registration_no_fees.tx > registration_no_fees.txid

jcli transaction to-message < registration_no_fees.tx > registration_no_fees.msg

echo "Pool ID: $(cat stake_pool.id)"
echo "This is the registration certificate:"
jcli certificate print stake_pool.cert.signed
