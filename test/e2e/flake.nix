{
  description = ''
    Shell for the Ruby E2E tests.
    This shell does *not* include `cardano-wallet` and `cardano-node`,
    only the Ruby environment.
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };

  outputs = inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        # The `blake2b` Ruby gem does not compile on aarch64-*
        # "aarch64-linux"
        # "aarch64-darwin"
       ]; in
    inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        # Imports
        pkgs = inputs.nixpkgs.legacyPackages.${system};

        # To update gemset.nix, run:
        #   nix develop -c bundix
        gems = pkgs.bundlerEnv {
          name = "gems-cardano-wallet-e2e";
          gemdir = ./.;
          ruby = pkgs.ruby_3_1;
        };
      in {
        packages = { };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.curl
            pkgs.jq
            gems
            gems.wrappedRuby
            pkgs.bundix
            pkgs.screen
          ];
          shellHook = ''
            # use this hook to set up additional environment variables
          '';
        };
      }
    );
}
