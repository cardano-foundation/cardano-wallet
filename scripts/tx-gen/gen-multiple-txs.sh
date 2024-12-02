#!/usr/bin/env bash
# Generate multiple deposit transactions
#
# This script will call gen-deposit.sh in a loop, reusing its output to generate
# more txs until the initial amount is exhausted.

set -e

[[ $# -eq 5 ]] || {
    echo "Usage: $0 <tx-in> <amount> <address> <key file> <increment> <interval>"
    exit 1
}

tx_in=$1
amount=$2
address=$3
keyfile=$4
increment=$5

# Mean interval in seconds
mean_interval=$6

generate_exponential() {
    rate=$(echo "1 / $mean_interval" | bc -l)

    # Generate a random number between 0 and 1 using /dev/urandom
    n=$(od -An -N4 -tu4 </dev/urandom)

    u=$(echo "$n /  4294967295" | bc -l)

    # Compute the exponential interval using the inverse CDF formula
    interval=$(echo "-l(1 - $u) / $rate" | bc -l)
    echo "$interval"
}

while [[ $amount -gt 1000000000 ]]; do
    # shellcheck disable=SC2207
    latest_tx=($(./gen-deposits.sh "$tx_in" "$amount" "$address" "$keyfile" "$increment"))
    amount=${latest_tx[1]}
    tx_in=${latest_tx[0]}
    # shellcheck disable=SC2012
    last_signed_tx=$(ls -1 tx.*.signed | sort -r -n -t '.' -k2 | head -1)

    if ! cardano-cli conway transaction submit --tx-file "${last_signed_tx}" --testnet-magic 1 --socket-path node.socket; then
        echo "Failed to submit ${last_signed_tx}"
        exit 1
    else
        echo "Submitted ${last_signed_tx} :" "${latest_tx[@]}"
    fi

    sleep "$(generate_exponential)"
done
