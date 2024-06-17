{ ... } @ args:
let
  src = args.src or ../.;
  lock = builtins.fromJSON (builtins.readFile "${src}/flake.lock");

  getInput = name: 
    let
      node = lock.nodes.${name};
    in 
    builtins.fetchTarball {
      url = "https://api.github.com/repos/${node.owner}/${node.repo}/tarball/${node.locked.rev}";
      sha256 = node.locked.narHash;
    };

  flakeCompatInput = lock.nodes.root.inputs.flake-compat;
  nixpkgsInput = lock.nodes.haskellNix.inputs.${builtins.elemAt lock.nodes.root.inputs.nixpkgs 1};

  flakeCompat = import (getInput flakeCompatInput);
  pkgs = import (getInput nixpkgsInput) {};

in flakeCompat {
  inherit src pkgs;
  override-inputs = {
    customConfig = args;
  };
}
