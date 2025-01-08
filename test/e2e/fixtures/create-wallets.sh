#!/usr/bin/env bash

set -euox pipefail

load_shelley() {
    local name=$1
    mnemonics=$(jq -r ".$name" fixture_wallets.json)

    json_data=$(
        cat <<EOF
{
  "name": "$name",
  "mnemonic_sentence": $mnemonics,
  "passphrase": "Secure Passphrase",
  "address_pool_gap": 20,
  "one_change_address_mode": false,
  "restoration_mode": "from_genesis"
}
EOF
    )

    curl \
        -X POST http://localhost:8090/v2/wallets \
        -H "Content-Type: application/json" \
        -d "$json_data"
}

shelleys="linux.fixture.shelley.mnemonics  \
    linux.target.shelley.mnemonics \
    macos.fixture.shelley.mnemonics \
    macos.target.shelley.mnemonics \
    windows.fixture.shelley.mnemonics \
    windows.target.shelley.mnemonics"
for name in $shelleys; do
    load_shelley "$name"
done

load_byron() {
    local name=$1
    mnemonics=$(jq -r ".$name" fixture_wallets.json)

    json_data=$(
        cat <<EOF
{
  "name": "$name",
  "style": "random",
  "mnemonic_sentence": $mnemonics,
  "passphrase": "Secure Passphrase",
  "address_pool_gap": 20,
  "one_change_address_mode": false,
  "restoration_mode": "from_seed"
}
EOF
    )

    curl \
        -X POST http://localhost:8090/v2/byron-wallets \
        -H "Content-Type: application/json" \
        -d "$json_data"
}


byrons="linux.fixture.random.mnemonics \
    macos.fixture.random.mnemonics \
    windows.fixture.random.mnemonics"

for name in $byrons; do
    load_byron "$name"
done

load_icarus() {
    local name=$1
    mnemonics=$(jq -r ".$name" fixture_wallets.json)

    json_data=$(
        cat <<EOF
{
  "name": "$name",
  "mnemonic_sentence": $mnemonics,
  "account_index": "2147483648",
  "passphrase": "Secure Passphrase",
  "address_pool_gap": 20,
  "one_change_address_mode": false,
  "restoration_mode": "from_seed",
  "style": "icarus"

}
EOF
    )

    curl \
        -X POST http://localhost:8090/v2/byron-wallets \
        -H "Content-Type: application/json" \
        -d "$json_data"
}


icaruses="linux.fixture.icarus.mnemonics \
    macos.fixture.icarus.mnemonics \
    windows.fixture.icarus.mnemonics"

for name in $icaruses; do
    load_icarus "$name"
done

# load_shared() {
#     local name=$1
#     mnemonics=$(jq -r ".$name" fixture_wallets.json)

#     json_data=$(
#         cat <<EOF
# {
#   "name": "$name",
#   "mnemonic_sentence": $mnemonics,
#   "account_index": "0",
#   "passphrase": "Secure Passphrase",
#   "address_pool_gap": 20,
#   "one_change_address_mode": false,
#   "restoration_mode": "from_seed"
# }
# EOF
#     )

#     curl \
#         -X POST http://localhost:8090/v2/shared-wallets \
#         -H "Content-Type: application/json" \
#         -d "$json_data"
# }

# shared="linux.fixture.shared.mnemonics \
#     linux.fixture.shared2.mnemonics \
#     macos.fixture.shared.mnemonics \
#     macos.fixture.shared2.mnemonics \
#     windows.fixture.shared.mnemonics \
#     windows.fixture.shared2.mnemonics"

# for name in $shared; do
#     load_shared "$name"
# done

curl \
    -X GET http://localhost:8090/v2/wallets | jq
