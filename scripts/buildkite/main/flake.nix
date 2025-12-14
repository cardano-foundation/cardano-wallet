{
  description = ''
    Shell for the main pipeline
    that builds release artifacts and runs E2E tests.
  '';

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    attic.url = "github:zhaofengli/attic";
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
        attic = inputs.attic.packages.${system}.default;
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
            pkgs.swagger-cli

            # Nix Caching
            attic
          ];
          shellHook = ''
            # use this hook to set up additional environment variables
          '';
        };
      }
    );
}
