#!/usr/bin/env -S nix shell nixpkgs#jq .#cardano-cli .#cardano-address -c bash
# shellcheck shell=bash

set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <mnemonic> <unsigned-tx-json>" >&2
    exit 1
fi

temp_dir=$(mktemp -d)

# cleanup function
cleanup() {
    rm -rf "$temp_dir"
}

trap cleanup EXIT
trap cleanup ERR
trap cleanup SIGINT

# collect arguments
mnemonic=$1
unsigned=$2

# create the root key
cardano-address key from-recovery-phrase Shelley <<<"$mnemonic" >"$temp_dir/root.xsk"

# extract bip32 paths from the unsigned tx json
paths=$(jq -r '.bip32Paths[]' <<<"$unsigned")

# derive keys, convert to cardano-cli format and collect --signing-key-file arguments
index=0
signing_key_files=""
for path in $paths; do
    key_file="$temp_dir/key${index}.xsk"
    cardano-address key child "$path" <"$temp_dir/root.xsk" >"$key_file"
    cli_key_file="$temp_dir/key${index}.skey"
    cardano-cli key convert-cardano-address-key \
        --shelley-payment-key \
        --signing-key-file "$key_file" \
        --out-file "$cli_key_file"
    signing_key_files="$signing_key_files --signing-key-file $temp_dir/key${index}.skey"
    index=$((index + 1))
done

# dump unsigned tx to a file
echo "$unsigned" >"$temp_dir/tx.unsigned"

# sign the transaction
# shellcheck disable=SC2086
cardano-cli conway transaction sign \
    $signing_key_files \
    --tx-body-file "$temp_dir/tx.unsigned" \
    --out-file "$temp_dir/tx.signed" \
    --testnet-magic 1

# print the signed transaction
cat "$temp_dir/tx.signed"
