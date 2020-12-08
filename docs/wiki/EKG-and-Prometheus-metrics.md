:warning: _in progress_ https://github.com/input-output-hk/cardano-wallet/pull/2322 :warning:

It is possible to enable EKG and Prometheus monitoring on cardano-wallet server, by setting environment variables that configure ports and host names for those services:
```
CARDANO_WALLET_EKG_PORT
CARDANO_WALLET_PROMETHEUS_PORT

CARDANO_WALLET_EKG_HOST
CARDANO_WALLET_PROMETHEUS_HOST
```
### Enable metrics
To enable monitoring one can simply set environment variables with `cardano-wallet serve` command as follows:
```
CARDANO_WALLET_EKG_PORT=6666 \
CARDANO_WALLET_PROMETHEUS_PORT=7777 \
cardano-wallet serve --port 8090 \
  --node-socket /path_to/cardano-node.socket \
  --mainnet \
  --database ./wallet-db
```
> :information_source: In order to see EKG `GC and memory statistics` start wallet with `cardano-wallet +RTS -T -RTS <other-args>`
> 
Following the example above metrics would be available in `localhost` under corresponding ports:
 - EKG: http://localhost:6666
 ```
 $ curl -H "Accept: application/json" http://localhost:6666/ | jq
{
  "iohk-monitoring version": {
    "type": "l",
    "val": "0.1.10.1"
  },
  "ekg": {
    "server_timestamp_ms": {
      "type": "c",
      "val": 1606997751752
    }
  },
  "rts": {
    "gc": {
      "gc_cpu_ms": {
        "type": "c",
        "val": 0
      },
...
 ```
 - Prometheus: http://localhost:7777/metrics  
 ```
 $ curl http://localhost:7777/metrics 
cardano_wallet_metrics_Stat_rtpriority_int 0
cardano_wallet_metrics_Stat_itrealvalue_int 0
rts_gc_par_max_bytes_copied 0
cardano_wallet_metrics_IO_syscr_int 3722
cardano_wallet_metrics_Stat_minflt_int 6731
cardano_wallet_metrics_Stat_cminflt_int 0
....
 ```
