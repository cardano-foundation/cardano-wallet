lib: customConfig: let config =
 lib.recursiveUpdate {
  platform = "all";
  # The systems that the jobset will be built for.
  supportedSystems = import ./nix/supported-systems.nix;

  # Can be "staging" or "trying" to indicate that this is a bors jobset
  borsBuild = "";

  # GitHub PR number (as a string), provided as a Hydra input
  pr = "";

  # Enable debug tracing
  debug = false;

  # Use this git revision for stamping executables
  gitrev = null;

  # Enable building the cabal-cache util - only needed under CI
  withCabalCache = false;

  # optional extra haskell.nix module
  haskellNix = {};

  # optional arguments for stack (nix-shell -A stack --arg stackExtraArgs '[]')
  stackExtraArgs = [];

  # optional string argument to override compiler, in cabal shell.
  ghcVersion = null;

} customConfig;
in
  assert lib.asserts.assertOneOf "platform" config.platform
    ["all" "linux" "macos" "windows"];
  config
