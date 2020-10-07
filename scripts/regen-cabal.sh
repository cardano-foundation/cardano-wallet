#!/usr/bin/env bash

# Make sure we error on subshell errors.
# https://unix.stackexchange.com/a/48550
# (more portable than 'shopt -s inherit_errexit')
set -E
trap '[ "$?" -ne 77 ] || exit 77' ERR


    ###########################
    #--[ Prelude functions ]--#
    ###########################

# @FUNCTION: error_message
# @USAGE: <msg>
# @DESCRIPTION:
# Print a red message.
error_message() {
  (>&2 printf "\\033[0;31m%s\\033[0m\\n" "$1")
}

# @FUNCTION: warning_message
# @USAGE: <msg>
# @DESCRIPTION:
# Print a yellow warning message.
warning_message() {
  (>&2 printf "\\033[1;33m%s\\033[0m\\n" "$1")
}

# @FUNCTION: status_message
# @USAGE: <msg>
# @DESCRIPTION:
# Print a green status message.
status_message() {
  (>&2 printf "\\033[0;32m%s\\033[0m\\n" "$1")
}

# @FUNCTION: die
# @USAGE: [msg]
# @DESCRIPTION:
# Exits the shell script with status code 77
# and prints the given message in red to STDERR, if any.
die() {
  error_message "$1"
  exit 77
}

# @FUNCTION: edo
# @USAGE: <command>
# @DESCRIPTION:
# Executes the given command.
# Exits with status code 77 if the command failed.
edo()
{
  "$@" || exit 77
}

# @FUNCTION: vercomp
# @USAGE: <verion 1> <version 2>
# @DESCRIPTION:
# Compares two versions for ordering.
# @RETURNS:
#   - 0 if equal
#   - 1 if version 1 is greater than version 2
#   - 2 if version 2 is greater than version 1
vercomp() {
  # https://stackoverflow.com/a/4025065

  [ "$#" -lt 2 ] && die "Internal error: not enough arguments given to vercomp"
  if [[ "$1" == "$2" ]] ; then
    return 0
  fi
  local IFS=.
  # This would break the array.
  # shellcheck disable=SC2206
  local i ver1=($1) ver2=($2)
  # fill empty fields in ver1 with zeros
  for ((i=${#ver1[@]}; i<${#ver2[@]}; i++)) ; do
    ver1[i]=0
  done
  for ((i=0; i<${#ver1[@]}; i++)) ; do
    if [[ -z ${ver2[i]} ]] ; then
        # fill empty fields in ver2 with zeros
        ver2[i]=0
    fi
    if ((10#${ver1[i]} > 10#${ver2[i]})) ; then
        return 1
    fi
    if ((10#${ver1[i]} < 10#${ver2[i]})) ; then
        return 2
    fi
  done

  return 0
}

# @FUNCTION: command_exists
# @USAGE: <command>
# @DESCRIPTION:
# Check if a command exists (no arguments).
# @RETURNS: 0 if the command exists, non-zero otherwise
command_exists() {
  [ -z "$1" ] && die "Internal error: no argument given to command_exists"

  command -v "$1" >/dev/null 2>&1
  return $?
}

# @FUNCTION: posix_realpath
# @USAGE: <file>
# @DESCRIPTION:
# Portably gets the realpath and prints it to stdout,
# since e.g. mac doesn't have realpath by default.
# This was initially inspired by
#   https://gist.github.com/tvlooy/cbfbdb111a4ebad8b93e
#   and
#   https://stackoverflow.com/a/246128
#
# If the file does not exist, just prints it appended to the current directory.
# @STDOUT: realpath of the given file
posix_realpath() {
    [ -z "$1" ] && die "Internal error: no argument given to posix_realpath"
    local current_loop=0
    local max_loops=50
    local mysource=$1
    local mydir

    while [ -h "${mysource}" ]; do
        current_loop=$((current_loop+1))
        mydir="$( cd -P "$( dirname "${mysource}" )" > /dev/null 2>&1 && pwd )"
        mysource="$(readlink "${mysource}")"
        [ "${mysource%${mysource#?}}"x != '/x' ] && mysource="${mydir}/${mysource}"

        if [ ${current_loop} -gt ${max_loops} ] ; then
            (>&2 echo "${1}: Too many levels of symbolic links")
            break
        fi
    done
    mydir="$( cd -P "$( dirname "${mysource}" )" > /dev/null 2>&1 && pwd )"

    if [ -z "${mydir}" ] ; then
        (>&2 echo "${1}: Permission denied")
    else
        echo "${mydir%/}/$(basename "${mysource}")"
    fi
}


    #######################
    #--[   Variables   ]--#
    #######################

# @VARIABLE: STACK2CABAL_VER
# @DESCRIPTION:
# The minimum stack2cabal version to accept.
# This is also the version that will be downloaded
# in case no appropriate version is found.
STACK2CABAL_VER="1.0.11"

# @VARIABLE: STACK2CABAL_LINUX_HASH
# @DESCRIPTION:
# The sha256sum of the stack2cabal linux binary.
STACK2CABAL_LINUX_HASH="24e49b44bdc69d218d941effbaca057b7de99425ff7623d8e61ed0393480dd4d"

# @VARIABLE: STACK2CABAL_MAC_HASH
# @DESCRIPTION:
# The sha256sum of the stack2cabal mac binary.
STACK2CABAL_MAC_HASH="f9410da4997b31ca1f3c553017bac200244b5edc7da8fc3ec02e3d150393fca1"

# @VARIABLE: SOURCE
# @DESCRIPTION:
# The $0 argument, which contains
# the script name.
SOURCE="$0"

# @VARIABLE: SCRIPT_DIR
# @DESCRIPTION:
# The absolute path of the directory,
# where this script resides in.
SCRIPT_DIR="$(dirname "$(posix_realpath "${SOURCE}")")"

# @VARIABLE: PROJECT_DIR
# @DESCRIPTION:
# The absolute path of the project directory,
# where the stack.yaml file naturally will be.
PROJECT_DIR="$(posix_realpath "${SCRIPT_DIR}"/../)"


    #######################
    #--[   Functions   ]--#
    #######################


# @FUNCTION: get_download_url
# @DESCRIPTION:
# Gets the download url of stack2cabal depending on the platform. Exits
# on unknown platform.
# @STDOUT: the download url
get_download_url() {
  case "$(uname -s)" in
    "linux"|"Linux")
      printf "https://github.com/hasufell/stack2cabal/releases/download/v%s/stack2cabal-linux" "${STACK2CABAL_VER}"
      ;;
    "Darwin"|"darwin")
      printf "https://github.com/hasufell/stack2cabal/releases/download/v%s/stack2cabal-macOS" "${STACK2CABAL_VER}"
      ;;
    *) die "Unknown plaftorm"
      ;;
  esac
}

# @FUNCTION: get_download_hash
# @DESCRIPTION:
# Gets the download hash of stack2cabal binary depending on the platform. Exits
# on unknown platform.
# @STDOUT: the download hash
get_download_hash() {
  case "$(uname -s)" in
    "linux"|"Linux")
      printf "%s" "${STACK2CABAL_LINUX_HASH}"
      ;;
    "Darwin"|"darwin")
      printf "%s" "${STACK2CABAL_MAC_HASH}"
      ;;
    *) die "Unknown plaftorm"
      ;;
  esac
}

# @FUNCTION: verify_checksum
# @USAGE: <expected-hash> <file>
# @DESCRIPTION:
# Verifies that the given file has the given hash. Does nothing
# on success, exits on mismatch or other errors.
verify_checksum() {
  [ "$#" -lt 2 ] && die "Internal error: not enough arguments given to verify_checksum"

  local expected_hash=$1
  local hash_file=$2

  status_message "Verifying stack2cabal hash"

  case "$(uname -s)" in
    "linux"|"Linux")
      [ "${expected_hash}" == "$(sha256sum "${hash_file}" | awk '{ print $1 }')" ] ||
        die "Hash mismatch"
      ;;
    "Darwin"|"darwin")
      [ "${expected_hash}" == "$(shasum -a 256 "${hash_file}" | awk '{ print $1 }')" ] ||
        die "Hash mismatch"
      ;;
    *) die "Unknown plaftorm"
      ;;
  esac
}

# @FUNCTION: install_stack2cabal
# @USAGE: <install-dir>
# @DESCRIPTION:
# Downloads and installs stack2cabal into the given directory.
install_stack2cabal() {
  [ -z "$1" ] && die "Internal error: no argument given to install_stack2cabal"
  local inst="$1"
  local dl_uri
  dl_uri="$(get_download_url)"

  status_message "Downloading stack2cabal"
  if command_exists curl ; then
    edo curl -L "${dl_uri}" > "${inst}"/stack2cabal
  elif command_exists wget ; then
    edo wget "${dl_uri}" -O "${inst}"/stack2cabal
  else
    die "Could neither find curl nor wget... giving up"
  fi

  verify_checksum "$(get_download_hash)" "${inst}"/stack2cabal

  edo chmod +x "${inst}"/stack2cabal
}

# @FUNCTION: ensure_stack2cabal
# @DESCRIPTION:
# Ensures an appropriate stack2cabal version is installed. First
# checks PATH for one, then checks <project-dir>/.tmp/bin and
# finally downloads stack2cabal binary if none exists.
# @STDOUT: the path to the binary
ensure_stack2cabal() {
  try_local_version() {
    edo mkdir -p "${PROJECT_DIR}"/.tmp/bin
    local inst_dir
    inst_dir="$(posix_realpath "${PROJECT_DIR}"/.tmp/bin)"

    if [ -x "${inst_dir}/stack2cabal" ] ; then
      status_message "Found local version of stack2cabal"
      vercomp "$("${inst_dir}/stack2cabal" --version)" "${STACK2CABAL_VER}"
      case $? in
        0) ;;
        1) ;;
        2) warning_message "stack2cabal version too old"
           install_stack2cabal "${inst_dir}" ;;
      esac
    else
      install_stack2cabal "${inst_dir}"
    fi
    printf "%s" "${inst_dir}/stack2cabal"
  }

  if command_exists stack2cabal ; then
    status_message "Found system version of stack2cabal"
    vercomp "$(stack2cabal --version)" "${STACK2CABAL_VER}"
    case $? in
      0) printf "%s" "$(command -v stack2cabal)";;
      1) printf "%s" "$(command -v stack2cabal)";;
      2) warning_message "stack2cabal version too old"
         try_local_version
         ;;
    esac
  else
    try_local_version
  fi
}


    ##########################
    #--[   Instructions   ]--#
    ##########################

if [ -e "${PROJECT_DIR}"/stack.yaml ] ; then
  bin="$(ensure_stack2cabal)"
  status_message "Executing stack2cabal"
  edo "${bin}" -f "${PROJECT_DIR}"/stack.yaml -o "${PROJECT_DIR}"/cabal.project -p now
else
  die "No stack.yaml in project directory found!"
fi

