#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils git

########################################################################
# This script creates/updates a branch in the cardano-wallet git repo
# to match the current HEAD revision.
#
# It's used to mark which revision has had all nightly tests
# successfully run.
#
#########################################################################

set -euo pipefail

branch="${1:-}"

if [ -z "$branch" ]; then
  echo "usage: $0 TARGET_BRANCH"
  exit 1
fi

: "${sshkey:=/run/keys/buildkite-cardano-wallet-ssh-private}"
remote="git@github.com:input-output-hk/cardano-wallet.git"

git fetch origin "$branch" || true

from=$(git show-ref -s "origin/$branch" || echo "-")
to=$(git show-ref -s HEAD)

echo "Advancing $branch from $from to $to"

if [ -e $sshkey ]; then
  echo "Authenticating using SSH with $sshkey"
  export GIT_SSH_COMMAND="ssh -i $sshkey -F /dev/null"
  git push $remote HEAD:refs/heads/$branch
  exit 0
else
  echo "There is no SSH key at $sshkey"
  echo "The update can't be pushed."
  exit 2
fi
