lib: customConfig: let config =
 lib.recursiveUpdate {
  platform = "all";
  # The systems that the jobset will be built for.
  supportedSystems = import ./nix/supported-systems.nix;

  # Enable debug tracing
  debug = false;

  # Use this git revision for stamping executables
  gitrev = null;

  # Enable building the cabal-cache util - only needed under CI
  withCabalCache = false;

  # optional extra haskell.nix module
  haskellNix = {};

  # optional string argument to override compiler, in cabal shell.
  ghcVersion = null;

  dockerHubRepoName = null;

} customConfig;
in
  assert lib.asserts.assertOneOf "platform" config.platform
    ["all" "linux" "macos" "windows"];
  config
