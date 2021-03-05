#!/usr/bin/env bash

exec nix-shell --run "bundle lock && bundix"
