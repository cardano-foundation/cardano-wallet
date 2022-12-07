let
  ciInputName = "GitHub event";
  repository = "input-output-hk/cardano-wallet";
in rec {
  tasks = let
    mkTask = top: system: path: { config, lib, pkgs, ... }: {
      preset = {
        nix.enable = true;

        github.ci = {
          enable = config.actionRun.facts != {};
          inherit repository;
          revision = config.preset.github.lib.readRevision ciInputName null;
        };
      };

      command.text = config.preset.github.status.lib.reportBulk {
        bulk.text = ''
          echo '["x86_64-linux", "x86_64-darwin"]' | nix-systems -i \
          | jq 'with_entries(.key |= {"x86_64-linux": "linux", "x86_64-darwin": "macos"}[.])'
        '';
        each.text = ''nix build -L .#${lib.escapeShellArg top}.${system}.${lib.escapeShellArg path}'';
        skippedDescription =  lib.escapeShellArg "No nix builder available for this platform";
      };

      dependencies = [ pkgs.jq ];

      # some hydra jobs run NixOS tests
      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 32;
      nomad.resources.cpu = 10000;
    };
  in
    {
      "ci/push" = mkTask "hydraJobs" ''"$1"'' "required";
      "ci/pr" = mkTask "hydraJobsPr" ''"$1"'' "required";
      "ci/integration" =
        mkTask "hydraJobsBors" "linux.musl" "checks.cardano-wallet.integration";
    };

  actions = {
    "cardano-wallet/ci/push" = {
      task = "ci/push";
      io = ''
        #lib.io.github_push
        #input: "${ciInputName}"
        #repo: "${repository}"
      '';
    };

    "cardano-wallet/ci/pr" = {
      task = "ci/pr";
      io = ''
        #lib.io.github_pr
        #input: "${ciInputName}"
        #repo: "${repository}"
        #target_default: false
      '';
    };

    "cardano-wallet/ci/integration" = {
      task = "ci/integration";
      io = ''
        #lib.io.github_pr
        #input: "${ciInputName}"
        #repo: "${repository}"
        #target_default: false
      '';
    }
  };
}
