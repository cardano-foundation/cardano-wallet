{
  description = ''
    Shell for the main pipeline
    that builds release artifacts and runs E2E tests.
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
            pkgs.rsync
            pkgs.gnutar
            pkgs.gnupg

            # Restore benchmark
            pkgs.buildkite-agent
            pkgs.gawk
            pkgs.gnuplot
            pkgs.gnugrep
            pkgs.haskellPackages.hp2pretty
            pkgs.time
          ];
          shellHook = ''
            # use this hook to set up additional environment variables
          '';
        };
      }
    );
}
