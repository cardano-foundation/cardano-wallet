{
  description = ''
    Shell for the admin.yml Buildkite pipeline
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
        "aarch64-linux"
        "aarch64-darwin"
       ]; in
    inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        # Imports
        pkgs = inputs.nixpkgs.legacyPackages.${system};
      in {
        packages = { };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.curl
            pkgs.jq
            pkgs.lz4
          ];
          shellHook = ''
            # use this hook to set up additional environment variables
          '';
        };
      }
    );
}
