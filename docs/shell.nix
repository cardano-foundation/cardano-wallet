{ ... }@args: (import ../nix/flake-compat.nix args).shellNix.devShells.${builtins.currentSystem}.docs
