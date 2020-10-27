# Fee Policy Override

## OS

Windows, MacOS, Linux

## No override

1. Create a valid `genesis.yaml` which does not define any `per_certificate_fees`

**genesis-no-override.yaml**
```yaml
blockchain_configuration:
  block0_date: 1556202057
  discrimination: test
  block0_consensus: genesis_praos
  slots_per_epoch: 100
  slot_duration: 5
  epoch_stability_depth: 10
  consensus_leader_ids:
    - ed25519_pk192m4ytl5k357e2l666yleuwjlurmf0vxjyh4atxzu5m22q6mexlsp88k7x
  bft_slots_ratio: 0
  consensus_genesis_praos_active_slot_coeff: 1
  max_number_of_transactions_per_block: 255
  kes_update_speed: 43200 # 12hours
  linear_fees:
    constant: 1337
    coefficient: 42
    certificate: 1
```

2. Encode it to produce a genesis block

```bash
$ jcli genesis encode --input-file genesis-no-override.yaml > block0-no-override.bin
```

3. Launch a wallet using that genesis configuration. The wallet should start and not fail.

```bash
$ cardano-wallet launch --genesis-block block0-no-override.bin
...
[iohk.cardano-wallet:Info:ThreadId 32] [2019-11-29 17:50:05.24 UTC] Wallet backend server listening on 127.0.0.1:8090
```

## With override

1. Create a valid `genesis.yaml` which does define a `per_certificate_fees`

**genesis-with-override.yaml**
```yaml
blockchain_configuration:
  block0_date: 1556202057
  discrimination: test
  block0_consensus: genesis_praos
  slots_per_epoch: 100
  slot_duration: 5
  epoch_stability_depth: 10
  consensus_leader_ids:
    - ed25519_pk192m4ytl5k357e2l666yleuwjlurmf0vxjyh4atxzu5m22q6mexlsp88k7x
  bft_slots_ratio: 0
  consensus_genesis_praos_active_slot_coeff: 1
  max_number_of_transactions_per_block: 255
  kes_update_speed: 43200 # 12hours
  linear_fees:
    constant: 1337
    coefficient: 42
    certificate: 1
    per_certificate_fees:
      certificate_pool_registration: 1000
      certificate_stake_delegation: 1000
      certificate_owner_stake_delegation: 1000
```

2. Encode it to produce a genesis block

```bash
$ jcli genesis encode --input-file genesis-with-override.yaml > block0-with-override.bin
```

3. Launch a wallet using that genesis configuration. The wallet should start and not fail.

```bash
$ cardano-wallet launch --genesis-block block0-with-override.bin
...
[iohk.cardano-wallet:Info:ThreadId 32] [2019-11-29 17:50:05.24 UTC] Wallet backend server listening on 127.0.0.1:8090
```
