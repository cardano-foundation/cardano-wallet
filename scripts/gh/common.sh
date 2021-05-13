#!/usr/bin/env bash
# shellcheck disable=SC2034  # Sourcing script uses variables

check_branch() {
  branch=$(git branch --show-current)
  if [ -n "$1" ] && [ "$1" != "$branch" ]; then
    echo "error: Current branch $branch is not the intended destination branch $1" > /dev/stderr
    exit 2
  fi
}

exit_unless_index_changed() {
  if git diff-index --cached --quiet HEAD --; then
    echo "No changes to commit, exiting."
    exit 0
  fi
}

commit_and_push() {
  exit_unless_index_changed
  git commit -m "$1$commit_msg_suffix"
  if [ -z "${NO_PUSH:-}" ]; then
    git push
  fi
}

export GIT_AUTHOR_NAME="William King Noel Bot"
export GIT_AUTHOR_EMAIL="adrestia@iohk.io"
export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME"
export GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"

commit_msg_suffix=" ${GITHUB_SHA:-update}"
dir=$(date +%Y-%m)
