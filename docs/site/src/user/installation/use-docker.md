# Running with Docker

Docker images are continuously built and deployed on [dockerhub](https://hub.docker.com/u/cardanofoundation) under specific tags. Using docker provides **the fastest** and **easiest** user experience for setting up the Cardano stack. You should prefer this solution over building from sources unless you have really good reasons not to. The following images are available for each component of the Adrestia architecture:

| Repository                                               |                 Tags                  |      Documentation       |
| :------------------------------------------------------- | :-----------------------------------: | :----------------------: |
| [inputoutput/cardano-node][inputoutput-cardano-node]     |  `master`, `MAJ.MIN.PATCH`, `latest`  | [link][doc-cardano-node] |
| [cardanofoundation/cardano-wallet][cardanofoundation-cardano-wallet] | `byron`, `YYYY.MM.DD-byron`, `latest` | [Docker](use-docker.md)  |

## Tag Naming Scheme

| Tag                             | Contents                                                                                                                                                                                                                                                                                                                                            |
| ------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `latest`                        | Points to the latest __stable__ image for the corresponding component. This is also the tag to which `docker` defaults when pulling without an explicit tag. These typically points to latest known release which happens at the end of an iteration cycle. Depending on which project / component, the iteration cycle may vary from 1 to 2 weeks. |
| `MAJ.MIN.PATCH` or `YYYY.MM.DD` | Must match actual releases of the corresponding component. Refer to each component release notes to know which release tags are available.                                                                                                                                                                                                          |
| `master`                        | Points to the very tip of the development branch. This is therefore __not recommended__ for production but can be useful to try out features before they are officially released.                                                                                                                                                                   |

### Examples

For example, in order to use `cardano-node@1.10.0`, one can simply run:

```
> docker pull inputoutput/cardano-node:1.10.0
```

Similarly, one can pull `cardano-wallet@v2021-08-11` with:

```
> docker pull cardanofoundation/cardano-wallet:2021.8.11
```

### About version compatibility

For version compatibility between components, please refer to compatibility matrix on each component main page
(e.g. [cardano-wallet](https://github.com/cardano-foundation/cardano-wallet#latest-releases)).

[DockerHub]: https://hub.docker.com/repository/docker/cardanofoundation/cardano-wallet

## Downloading the Docker image

To get the latest release of `cardano-wallet`, run:

    docker pull cardanofoundation/cardano-wallet:latest

## Running the Docker container for cardano-wallet

To run basic CLI commands, use:

```
> docker run --rm cardanofoundation/cardano-wallet:latest --help
```

See [cli](../cli.md)
 for full documentation of the CLI.

## Building the Docker image locally

Ensure that you have Nix installed and the IOHK binary cache enabled
([instructions](https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix.md)).

Then run this command from the `cardano-wallet` git repo:

```
> docker load -i $(nix build --json .#dockerImage | jq -r '.[0].outputs.out')
```

If you have no local changes, the build should be downloaded from
the [IOHK binary cache](https://hydra.iohk.io/job/Cardano/cardano-wallet/native.dockerImage.shelley.x86_64-linux)
then loaded into your local Docker image storage.

## Inspecting the contents of the Docker image

The default entrypoint of the image is
`/bin/start-cardano-wallet-shelley`. If you need to run a shell
inside the Docker image, use the bash shell as the entrypoint:

```
> docker run --rm -it --entrypoint bash cardanofoundation/cardano-wallet:latest
```

## Docker compose

One can also use [docker-compose](https://docs.docker.com/compose/) to quickly spin up `cardano-wallet` together with supported block producer. Those are useful for a quick start or as a baseline for development.

[cardano-wallet/docker-compose.yml](https://github.com/cardano-foundation/cardano-wallet/blob/master/docker-compose.yml) is an example `docker-compose.yaml` combining the latest versions of `cardano-wallet` and `cardano-node`.

To give it a spin, simply launch:

```
wget https://raw.githubusercontent.com/cardano-foundation/cardano-wallet/master/docker-compose.yml
NETWORK=mainnet docker-compose up
```

There is also an example configuration for [cardano-graphql](https://github.com/cardano-foundation/cardano-graphql/blob/master/docker-compose.yml).

[inputoutput-cardano-node]: https://hub.docker.com/r/inputoutput/cardano-node
[inputoutput-cardano-db-sync]: https://hub.docker.com/r/inputoutput/cardano-db-sync
[inputoutput-cardano-graphql]: https://hub.docker.com/r/inputoutput/cardano-graphql
[inputoutput-cardano-wallet]: https://hub.docker.com/r/cardanofoundation/cardano-wallet
[inputoutput-cardano-rosetta]: https://hub.docker.com/r/inputoutput/cardano-rosetta

[doc-cardano-node]: https://github.com/IntersectMBO/cardano-node/blob/master/nix/docker.nix#L1-L25
