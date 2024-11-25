#!/usr/bin/env bash

usage () {
  cat <<'EOF'
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

## Requirements

The deposit outputs are computed from a file `root.prv` which is expected to be a
root master signing key, eg. restored from mnemonics for example:

```
% cardano-address key from-recovery-phrase Shelley  <<< "..." > root.prv
```

The script also requires a 'protocol-params.json' in the current directory, which can
be retrieved directly from the network with

```
cardano-wallet % cardano-cli conway query protocol-parameters --socket-path databases/node.socket --testnet-magic 1 > protocol-params.json
```

## Examples

Generating a transaction with lots of outputs, assuming input UTxO has enough value:

```
 ./gen-deposits.sh '5896d53c996f437874d5e9edd56213021bae49dab1ef86ec3908b148ad009f99#0' 1000000000000 addr_test1vpf26vappzgz5zvjv362pzaag27c4dg4tu7qq7n07e686dsy5669m txgen.sk
...
built transaction with 212 outputs (15909B)
built transaction with 213 outputs (15983B)
built transaction with 214 outputs (16057B)
tx too large
build transaction 0: 164972f37d3efcf156ca6ef968b89395f39dd886425b64737bce9361fa2e4412
remaining ₳895184
```

Generating a transaction given an input with not much value:

```
% ./gen-deposits.sh '5896d53c996f437874d5e9edd56213021bae49dab1ef86ec3908b148ad009f99#0' 10000000000 addr_test1vpf26vappzgz5zvjv362pzaag27c4dg4tu7qq7n07e686dsy5669m txgen.sk
...
built transaction with 15 outputs (1395B)
built transaction with 16 outputs (1469B)
Command failed: transaction build-estimate  Error: The transaction does not balance in its use of ada. The net balance of the transaction is negative: -60191857 Lovelace. The usual solution is to provide more inputs, or inputs with more ada.
failed to build txs with 17 outputs
build transaction 0: 4b1ff833e358093f8dad9eb96f7bf34621fc183da3b0e06a398decf3133e6d38
remaining ₳860
```

Unless the script fails at the very first output it tries to add, it should always
generate a `tx.0.signed` transaction that can be submitted.

```
% cardano-cli conway transaction submit --tx-file tx.0.signed --testnet-magic 1 --socket-path node.socket
```

EOF
}

if [[ $# -ne 4 ]]; then
  usage
  exit 2
fi

# generate random 32-bit numbers
random_u32=$(dd if=/dev/urandom count=1000 bs=4 2> /dev/null | od -t u4  | tr -s ' ' | cut -d ' ' -f2)

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

  if ! cardano-cli conway transaction build-estimate $args; then
    echo "failed to build txs with ${#current_addresses[@]} outputs"
    return 1 # retry
  else
    tx_size=$(ls -l ${tmp_raw} | tr -s ' ' | cut -d ' ' -f 5)

    # check the size of the raw transaction
    if [[ tx_size -gt $((MAX_TX_SIZE - 300)) ]]; then
      echo "tx too large"
      return 1 #retry
    else
      cp ${tmp_raw} ${raw}
      echo "built transaction with ${#current_addresses[@]} outputs (${tx_size}B)"
      cardano-cli conway transaction sign --tx-file ${raw} --signing-key-file ${signing_key_file} --testnet-magic 1 --out-file ${signed}
    fi

  fi
}

for r in $random_u32; do
  # account id between 0 and 100000
  acct_id=$((r % 10 ))

  # derive public address
  account_address=$(cat root.prv | cardano-address key child "1852H/1815H/0H/0/${acct_id}" | cardano-address key public --with-chain-code | cardano-address address payment --network-tag preprod)

  # generate an amount of ADA between 100 and 10000, in lovelaces (x 1000000)
  rand=$(dd if=/dev/urandom count=4 bs=1 2> /dev/null | od -t u4 | tr -s ' ' | cut -d ' ' -f2 | head -1)
  adas=$(( 5 + rand % 1000 ))

  current_adas+=( $adas )
  current_addresses+=( $account_address )

  # try to construct a tx paying to all current addresses
  try_building_tx
  if [[ $? -eq 1 ]] ; then
    signed=tx.${tx_num}.signed
    echo "build transaction ${tx_num}: $(cardano-cli conway transaction txid --tx-file ${signed})"
    tx_num=$(( $tx_num + 1 ))
    total_utxo_value=$(cardano-cli debug transaction view --tx-file ${signed} | jq ".outputs[] | select ( .address == \"${change_address}\") | .amount.lovelace")
    echo "remaining ₳$(( ${total_utxo_value} / 1000000 ))"
    # need to retry
    break
  fi
done
