#!/usr/bin/env bash
sleep $[ ( $RANDOM % 2 ) + 1 ]s;
exit $1;
