# This file is used by nix-shell.
# It just plucks the "shell" attribute from default.nix.
# See that file for more info.

let cardanoWallet = import ./default.nix {};

# If you want Haskell dependencies built with profiling, then use:
# nix-shell --arg profiling true
in { profiling ? false }:

  if profiling
    then cardanoWallet.shell-prof
    else cardanoWallet.shell
