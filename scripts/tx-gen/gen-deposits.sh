#!/usr/bin/env bash

usage () {
  cat <<EOF
Usage: ./gen-deposits.sh <tx-in>  <utxo value> <change-address> <signing key file>

A naive script to generate and sign deposit transactions to random accounts in a wallet.

Arguments:
* <tx-in>: A transaction input containing ₳ to distribute funds from
* <utxo value>: The value locked at tx-in
* <change-address>: Address to return change to. This is expected to be the same address
  than the one the tx-in points to
* <signing key file>: the signing key used to sign the transaction.

This script will try to accumulate as many payment outputs to some random accounts for
a wallet whose root private key is in file 'root.prv' in a single transaction, until
either the funds of the input tx-in (plus fees) run out or the size of the transaction
goes above current protocol parameters limit.

The script also requires a 'protocol-params.json' in the current directory, which can
be retrieved directly from the network with

```
cardano-wallet % cardano-cli conway query protocol-parameters --socket-path databases/node.socket --testnet-magic 1 > protocol-params.json
```

EOF
}

if [[ $# -ne 4 ]]; then
  usage
  exit 2
fi

# generate random 32-bit numbers
random_u32=$(dd if=/dev/urandom count=800 bs=4 2> /dev/null | od -t u4  | tr -s ' ' | cut -d ' ' -f2)

# start from a single tx input
current_tx_in=$1

# can be extracted from
# cardano-cli conway query utxo  --address ${change_address} --testnet-magic 1 --socket-path node.socket| tail -n 1 | tr -s ' ' | cut -d ' ' -f 3
total_utxo_value=$2

# use this as change address
change_address=$3

# the signing key
signing_key_file=$4

declare current_adas
declare current_addresses

MAX_TX_SIZE=$(cat protocol-params.json| jq .maxTxSize)

tx_num=0

try_building_tx () {
  raw=tx.${tx_num}.raw
  tmp_raw=${raw}.tmp
  signed=tx.${tx_num}.signed

  args="--protocol-params-file protocol-params.json --shelley-key-witnesses 1 --change-address ${change_address} --out-file ${tmp_raw} --tx-in ${current_tx_in}"

  for i in ${!current_adas[@]}; do
    args+=" --tx-out ${current_addresses[$i]}+$(( ${current_adas[$i]} * 1000000 ))"
  done

  args+=" --total-utxo-value ${total_utxo_value}"
  echo "building transaction with ${#current_addresses[@]} outputs"

  if ! cardano-cli conway transaction build-estimate $args; then
    total_utxo_value=$(cardano-cli debug transaction view --tx-file ${raw} | jq ".outputs[] | select ( .address == \"${change_address}\") | .amount.lovelace")
    tx_num=$(( $tx_num + 1 ))
    echo "failed to build transaction probably because amount is too high ${total_utxo_value}"
    exit 1 # retry
  else
    tx_size=$(ls -l ${raw} | tr -s ' ' | cut -d ' ' -f 5)

    # check the size of the raw transaction
    if [[ tx_size -gt $((MAX_TX_SIZE - 300)) ]]; then
      echo "tx too large"
      total_utxo_value=$(cardano-cli debug transaction view --tx-file ${signed} | jq '.outputs[] | select ( .address == "${change_address}") | .amount.lovelace')
      tx_num=$(( $tx_num + 1 ))
      exit 1 #retry
    else
      cp ${tmp_raw} ${raw}
      cardano-cli conway transaction sign --tx-file ${raw} --signing-key-file ${signing_key_file} --testnet-magic 1 --out-file ${signed}
    fi

  fi
}

for r in $random_u32; do
  # account id between 0 and 100000
  acct_id=$((r % 10 ))

  # derive public address
  account_address=$(cat root.prv | cardano-address key child "1852H/1815H/${acct_id}H/0/0" | cardano-address key public --with-chain-code | cardano-address address payment --network-tag preprod)

  # generate an amount of ADA between 100 and 10000, in lovelaces (x 1000000)
  rand=$(dd if=/dev/urandom count=4 bs=1 2> /dev/null | od -t u4 | tr -s ' ' | cut -d ' ' -f2 | head -1)
  adas=$(( 5 + rand % 1000 ))

  echo "sending ₳$adas to $account_address"
  current_adas+=( $adas )
  current_addresses+=( $account_address )

  # try to construct a tx paying to all current addresses
  try_building_tx
  if [[ $? -eq 1 ]] ; then
    # need to retry
    try_building_tx
  fi
done

echo ${current_adas[@]}
echo ${current_addresses[@]}
