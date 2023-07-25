# Swagger

The OpenAPI 3.0 (Swagger) specification of cardano-wallet is located at [specifications/api/swagger.yaml](https://github.com/cardano-foundation/cardano-wallet/blob/master/specifications/api/swagger.yaml).


## Viewing online

To view the file online, use:

- [http-api](../user-guide/http-api.md)
    for the latest released version.

- [ReDoc viewer](https://redocly.github.io/redoc/?url=https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/specifications/api/swagger.yaml) for the `master` branch version.


## Validating locally

To validate from your local git working tree, use this command. It is the same as what [Buildkite](https://github.com/cardano-foundation/cardano-wallet/blob/master/.buildkite/pipeline.yml) runs.

    openapi-spec-validator --schema 3.0.0 specifications/api/swagger.yaml

If you don't have the validator tool installed, it is available within `nix-shell`.

It may also be convenient to edit the YAML while validating under the [Steel Overseer](https://github.com/schell/steeloverseer#readme) file watching tool:

    sos specifications/api/swagger.yaml -c 'openapi-spec-validator --schema 3.0.0 \0'


## References

- [Swagger Docs](https://swagger.io/docs/)

- [Understanding JSON Schema](http://json-schema.org/understanding-json-schema/index.html)

- [YAML spec](https://yaml.org/spec/1.2/spec.html)
