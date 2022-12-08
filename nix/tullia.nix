let
  ciInputName = "GitHub event";
  repository = "input-output-hk/cardano-wallet";
in rec {
  tasks = let
    mkTask = top: { config, lib, pkgs, ... }: {
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
        each.text = ''nix --version'';
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
      "ci/push" = mkTask "hydraJobs";
      "ci/pr" = mkTask "hydraJobsPr";
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
  };
}
