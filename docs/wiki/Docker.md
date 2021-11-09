Docker builds of `cardano-wallet` are available on [DockerHub][].

[DockerHub]: https://hub.docker.com/repository/docker/inputoutput/cardano-wallet

## Downloading the Docker image

To get the latest release of `cardano-wallet`, run:

    docker pull inputoutput/cardano-wallet:latest

## Running the Docker container for `cardano-wallet`

To run basic CLI commands, use:

```
docker run --rm inputoutput/cardano-wallet:latest --help
```

See [Wallet Command Line Interface](./Wallet-command-line-interface)
for full documentation of the CLI.

## Naming scheme

The following tags are pushed to [DockerHub][].

| Tag                                                 | Network node backend        | Version          |
|:----------------------------------------------------|:---------------------------:|:-----------------|
| `inputoutput/cardano-wallet`                        | cardano-node  | same as _latest_ |
| `inputoutput/cardano-wallet:latest`                | cardano-node  | Latest [GitHub release](https://github.com/input-output-hk/cardano-wallet/releases) |
| `inputoutput/cardano-wallet:dev-master`     | cardano-node  | Latest revision of [master branch](https://github.com/input-output-hk/cardano-wallet/commits/master) |
| `inputoutput/cardano-wallet:2020.7.28`      | cardano-node  | [v2020-07-28](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-07-28) (for example) |

## Building the Docker image locally

Ensure that you have Nix installed and the IOHK binary cache enabled
([instructions](https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix.md)).

Then run this command from the `cardano-wallet` git repo:

```
docker load -i $(nix-build -A dockerImage --no-out-link)
```

If you have no local changes, the build should be downloaded from
the [IOHK binary cache](https://hydra.iohk.io/job/Cardano/cardano-wallet/native.dockerImage.shelley.x86_64-linux)
then loaded into your local Docker image storage.

## Inspecting the contents of the Docker image

The default entrypoint of the image is
`/bin/start-cardano-wallet-shelley`. If you need to run a shell
inside the Docker image, use the bash shell as the entrypoint:

```
docker run --rm -it --entrypoint bash inputoutput/cardano-wallet:latest
```

## Docker compose

One can also use [docker-compose](https://docs.docker.com/compose/) to quickly spin up `cardano-wallet` together with supported block producer.

[Here](https://github.com/input-output-hk/cardano-wallet/blob/master/docker-compose.yml) is exemplary `docker-compose.yaml` combining the latest versions of `cardano-wallet` and `cardano-node`.

To give it a spin simply launch:

```
wget https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/docker-compose.yml
NETWORK=mainnet docker-compose up
```
