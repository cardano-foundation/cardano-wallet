#!/usr/bin/env bash
netstat -nlp | grep -E 'tcp.*'
