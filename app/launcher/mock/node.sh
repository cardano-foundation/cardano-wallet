#!/usr/bin/env bash
while true; do echo "I'm a node"; sleep $[ ( $RANDOM % 2 )  + 1 ]s; done
