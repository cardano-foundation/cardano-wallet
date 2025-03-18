#!/usr/bin/env bash

set -euo pipefail

# define command as the script parameter or default to check
command=${1:-check}

# traverse the directories and apply the operation to the files that match the regex
traverse() {
    local op=$1
    local directory=$2
    local regex=$3
    find "$directory" -type f -regex "$regex" |
        while read -r file; do
            $op "$file"
        done
}

# check if the file ends with a newline character and, depending on the command, add it
# or bail out with an error message
check() {
    local file=$1
    if [ -s "$file" ] && [ "$(tail -c 1 "$file")" != "" ]; then
        case $command in
        add)
            echo "Adding newline character to $file"
            echo "" >>"$file"
            ;;
        check)
            echo "File $file does not end with a newline character."
            echo "Fix this by running ./scripts/enforce-eol.sh add"
            exit 1
            ;;
        *)
            echo "Invalid command: $command"
            echo "Usage: enforce-eol.sh [check|add]"
            exit 1
            ;;
        esac
    fi
}

directories=("lib" "scripts" "docs" "nix" "specifications" "configs")

# json is not included because most of json is golden files. It would be better to
# use the directories filter instead of the regex filter
regex=".*\.\(md\|hs\|sh\|nix\|yaml\|yml\|toml\|cabal\)$"

for directory in "${directories[@]}"; do
    traverse check "$directory" "$regex"
done
