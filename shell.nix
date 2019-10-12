let
  self = import ./default.nix {};
  shell = self.shell;
  devops = self.devopsShell;

in (import ./default.nix {}).shell // { inherit devops; }
