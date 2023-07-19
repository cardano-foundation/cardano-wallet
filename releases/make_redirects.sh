#! /usr/bin/env bash

VER=$1

mkdir -p $VER/user-guide
echo '<meta http-equiv="refresh" content="0;url=/cardano-wallet/user-guide/cli" />' >> $VER/user-guide/cli.html
echo '<meta http-equiv="refresh" content="0;url=/cardano-wallet/user-guide/Docker" />' >> $VER/user-guide/Docker.html
echo "<meta http-equiv=\"refresh\" content=\"0;url=/cardano-wallet/api/$VER/\" />" >> $VER/api.html

git add $VER
read -p "Do you want to create a commit (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
  git commit -am "Redirects for $VER"
fi
