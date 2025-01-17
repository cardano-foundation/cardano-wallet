#!/usr/bin/env bash

set -euox pipefail

swagger-cli validate specifications/api/swagger.yaml
