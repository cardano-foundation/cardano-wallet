#!/usr/bin/env bash
while true; do
  sleep $[ ( $RANDOM % 2 )  + 1 ]s
done
