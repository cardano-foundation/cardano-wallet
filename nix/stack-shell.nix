{ extraArgs ? [], stackExtraArgs ? extraArgs, ...}@args: (import ./flake-compat.nix args).shellNix.stack
