#!/usr/bin/env bash
echo "I'm a wallet (will exit 1)";
sleep $[ ( $RANDOM % 2 )  + 1 ]s;
exit 1;
