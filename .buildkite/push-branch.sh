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

this_branch="${1:-}"
other_branch="${2:-}"
common_branch="${3:-}"

if [ -z "$common_branch" ]; then
  echo "usage: $0 THIS_BRANCH OTHER_BRANCH COMMON_BRANCH"
  exit 1
fi

# Load SSH key from GitHub secrets if run under GitHub actions.
if [ -n "${ACTIONS_SSH_KEY:-}" ]; then
  sshkey=$(mktemp)
  echo "${ACTIONS_SSH_KEY:-}" > $sshkey
fi

# Load SSH key from standard location on Buildkite.
: "${sshkey:=/run/keys/buildkite-cardano-wallet-ssh-private}"

# SSH name of our git repo
remote="git@github.com:input-output-hk/cardano-wallet.git"

advance_branch() {
  branch="$1"
  to="$2"

  from=$(git show-ref -s "origin/$branch" || echo "-")

  echo "Advancing $branch from $from to $to"
}

git fetch origin "$this_branch" || true
git fetch origin "$other_branch" || true

advance_branch "$this_branch" $(git show-ref -s HEAD)

common_ref=$(git merge-base "$this_branch" "$other_branch" || true)

if [ -n "$common_ref" ]; then
  advance_branch "$common_branch" "$common_ref"
fi

if [ -e $sshkey ]; then
  echo "Authenticating using SSH with $sshkey"
  export GIT_SSH_COMMAND="ssh -i $sshkey -F /dev/null"
  git push $remote HEAD:refs/heads/$this_branch
  if [ -n "$common_ref" ]; then
    git push $remote "$common_ref":refs/heads/$common_branch
  fi
  exit 0
else
  echo "There is no SSH key at $sshkey"
  echo "The update can't be pushed."
  exit 2
fi
