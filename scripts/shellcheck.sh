#! /usr/bin/env nix-shell
#! nix-shell -i bash -p shellcheck figlet lolcat
# shellcheck shell=bash

# NOTE: it can help to run this under the steel overseer when writing
# a script. Like this:
#
#   sos scripts '.*\.sh$' -e '\#' -c ./scripts/shellcheck.sh
#

scripts_dir="${0%/*}"

if [ -z "${1:-}" ]; then
  mapfile -t files < <(find "$scripts_dir" -type f -name '*.sh')
else
  files=( "$@" )
fi

if shellcheck -e SC1090 --external-sources --source-path="$scripts_dir" --source-path="$scripts_dir/gh" "${files[@]}"; then
  echo "Basic linting of ${#files[@]} shell scripts passed. 👍"
  echo "awesome!" | figlet | lolcat
else
  # Begin patronising advice
  echo
  echo -e "\e[31mBasic linting of shell scripts failed!\e[0m"
  echo
  echo "REMINDER:"
  echo "  Avoid trying to be too clever with shell scripts."
  echo "  Writing the script in Haskell could actually be easier."
  echo
  # End
  exit 1
fi
