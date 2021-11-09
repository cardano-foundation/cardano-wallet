# Docker Image

Docker builds of `cardano-wallet-jormungandr` are available on [DockerHub][].

[DockerHub]: https://hub.docker.com/repository/docker/inputoutput/cardano-wallet

## Downloading the Docker image

To get the latest release of `cardano-wallet-itn`, run:

    docker pull inputoutput/cardano-wallet:jormungandr

## Running the Docker container for `cardano-wallet-itn`

To run basic CLI commands, use:

```
docker run --rm inputoutput/cardano-wallet:jormungandr --help
```

See [[cli]] for full documentation of the CLI.

Running a `cardano-wallet` server requires setting up a state directory with a jormungandr config file.

This example uses the [itn_rewards_v1](https://hydra.iohk.io/job/Cardano/iohk-nix/jormungandr-deployment/latest/download/1/index.html) testnet.

1. Set up a state directory with the network config on the Docker host.

   ```
   mkdir $HOME/state-docker
   wget -P $HOME/state-docker https://hydra.iohk.io/job/Cardano/iohk-nix/jormungandr-deployment/latest/download/1/itn_rewards_v1-config.yaml
   ```

2. Run the `cardano-wallet` server and `jormungandr`:

   ```
   docker run \
       --publish 127.0.0.1:8090:8090 \
       --volume $HOME/state-docker:/data \
       --env TMPDIR=/data \
       --rm \
       inputoutput/cardano-wallet \
       launch \
       --listen-address 0.0.0.0 \
       --state-dir /data \
       --genesis-block-hash 8e4d2a343f3dcf9330ad9035b3e8d168e6728904262f2c434a4f8f934ec7b676 \
       -- --config /data/itn_rewards_v1-config.yaml
   ```

   Explanation of the arguments:

   * `--publish 127.0.0.1:8090:8090` - exposes the API server port from the
     container to the Docker host.

   * `--volume $HOME/state-docker:/data` - mounts the
     `~/state-docker` directory on the Docker host to `/data` inside
     the container.

   * `--env TMPDIR=/data` - work around a temp directory issue
     ([\#1262](https://github.com/input-output-hk/cardano-wallet/pull/1262)).
     Alternatively, you may mount an additional volume on your own temporary
     directory with `--volume /tmp:/tmp`.

   * `--listen-address 0.0.0.0` - ensures that the API server binds to
     the Docker network interface, so that the server can be accessed
     from outside of the container.

   * `--state-dir /data` - use the data volume for chain and wallet databases.


3. Wait for the log message `Wallet backend server listening on
   0.0.0.0:8090` to be shown (it may take 30-60 minutes for
   jormungandr to bootstrap).

   Then in another shell, on the Docker host, run:

   ```
   curl http://localhost:8090/v2/network/information
   ```

   The cardano-wallet API server should return its status as JSON.


## Naming scheme

The following tags are pushed to [DockerHub][].

| Tag                                                 | Network node backend        | Version          |
|:----------------------------------------------------|:---------------------------:|:-----------------|
| `inputoutput/cardano-wallet:jormungandr`            | Jörmungandr                 | Latest [GitHub release](https://github.com/input-output-hk/cardano-wallet/releases) |
| `inputoutput/cardano-wallet:dev-master-jormungandr` | Jörmungandr                 | Latest revision of [master branch](https://github.com/input-output-hk/cardano-wallet/commits/master) |
| `inputoutput/cardano-wallet:2020.1.7-jormungandr`   | Jörmungandr                 | [v2020-01-07](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-01-07) (for example) |
| `inputoutput/cardano-wallet:e902913750f763ac3dcb37f57fa35154db2934eb-jormungandr` | Jörmungandr        | A certain revision of the master branch (e902913 for example). |

## Building the Docker image locally

Ensure that you have Nix installed and the IOHK binary cache enabled
([instructions](https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix.md)).

Then run this command from the `cardano-wallet` git repo:

```
docker load -i $(nix-build -A dockerImage.jormungandr --no-out-link)
```

If you have no local changes, the build should be downloaded from
the [IOHK binary cache](https://hydra.iohk.io/job/Cardano/cardano-wallet/native.dockerImage.jormungandr.x86_64-linux)
then loaded into your local Docker image storage.

## Inspecting the contents of the Docker image

The default entrypoint of the image is
`/bin/start-cardano-wallet-jormungandr`. If you need to run a shell
inside the Docker image, use the bash shell as the entrypoint:

```
docker run --rm -it --entrypoint bash inputoutput/cardano-wallet:jormungandr
```
