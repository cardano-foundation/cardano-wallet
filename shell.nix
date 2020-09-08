# This file is used by nix-shell.
# It just plucks the "shell" attribute from default.nix.
# See that file for more info.
#
# If you want Haskell dependencies built with profiling, then use:
# nix-shell --arg profiling true
{ profiling ? false
, sourcesOverride ? {} # see sourcesOverride in nix/default.nix
}:

let
   cardanoWallet = import ./default.nix { inherit sourcesOverride; };
in
  if profiling
    then cardanoWallet.shell-prof
    else cardanoWallet.shell
