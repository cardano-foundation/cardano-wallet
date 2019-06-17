# This is a a CI check script that runs nix/regenerate.sh in the
# project repository. If anything changes, then it uploads the patch
# to Buildkite. If run on a PR branch, and there is a SSH key present,
# it will attempt to push the changes back to the PR.

{ stdenv, writeScript, coreutils, nixStable, git, gawk }:

with stdenv.lib;

writeScript "check-nix-tools.sh" ''
  #!${stdenv.shell}

  set -euo pipefail

  export PATH="${makeBinPath [ stdenv.shellPackage coreutils nixStable git gawk ]}:$PATH"

  cd $(git rev-parse --show-toplevel)

  # The regenerate script is here by convention
  ./nix/regenerate.sh

  # The generated files will appear somewhere under ./nix
  git add -A nix

  # Check if there are changes staged for commit.
  if git diff-index --cached --quiet HEAD --; then
    echo "Generated Nix code is up-to-date."
    exit 0
  else
    echo "Committing changes..."
    commit_message="Regenerate nix"

    # If on a PR branch, search for a previous regen commit to fix up.
    commit_fixup=""
    if [ -n "''${BUILDKITE_PULL_REQUEST_BASE_BRANCH:-}" ]; then
      git fetch -v origin $BUILDKITE_PULL_REQUEST_BASE_BRANCH
      commit_fixup=$(git log --pretty=oneline --no-decorate origin/$BUILDKITE_PULL_REQUEST_BASE_BRANCH..HEAD | awk "/$commit_message/ { print \$1; }")
    fi

    # Create the commit
    export GIT_COMMITTER_NAME="IOHK"
    export GIT_COMMITTER_EMAIL="devops+nix-tools@iohk.io"
    export GIT_AUTHOR_NAME="$GIT_COMMITTER_NAME"
    export GIT_AUTHOR_EMAIL="$GIT_COMMITTER_EMAIL"
    if [ -n "$commit_fixup" ]; then
      git commit --no-gpg-sign --fixup "$commit_fixup"
    else
      git commit --no-gpg-sign --message "$commit_message"
    fi

    # If running in Buildkite...
    if [ -n "''${BUILDKITE_JOB_ID:-}" ]; then

      # Upload the patch as a Buildkite artifact
      patch="$BUILDKITE_PIPELINE_SLUG-nix-$BUILDKITE_BUILD_NUMBER.patch"
      git format-patch --stdout -1 HEAD > "$patch"
      buildkite-agent artifact upload "$patch" --job "$BUILDKITE_JOB_ID"

      # Push the changes back to the pull request
      if [ -n "''${BUILDKITE_PULL_REQUEST_REPO:-}" ]; then
        sshkey="/run/keys/buildkite-$BUILDKITE_PIPELINE_SLUG-ssh-private"
        if [ -e $sshkey ]; then
          echo "Authenticating using SSH with $sshkey"
          export GIT_SSH_COMMAND="ssh -i $sshkey -F /dev/null"
          remote=$(echo $BUILDKITE_PULL_REQUEST_REPO | sed -e 's=^[a-z]*://github.com/=git@github.com:=')
          git push $remote HEAD:$BUILDKITE_BRANCH
          exit 0
        else
          echo "There is no SSH key at $sshkey"
          echo "The updates can't be pushed."
          echo "Apply the patch $patch from the build artifacts"
        fi
      fi

    fi

    exit 1
  fi
''
