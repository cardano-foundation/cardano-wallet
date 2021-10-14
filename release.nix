############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
# The layout is PLATFORM.ATTR-PATH where
#
#   * PLATFORM is one of
#     - linux.native - normal glibc build
#     - linux.musl - fully static build with musl libc
#     - windows - cross-compiled windows
#     - musl64 - build the job for Linux, but statically linked with musl libc
#
#   * ATTR-PATH is usually an attribute from default.nix. Some
#     release-only attributes are added and others are filtered out.
#
# Discover jobs by using tab completion in your shell:
#   nix-build release.nix -A <TAB>
# ... or by looking at the jobset evaluated by Hydra:
#   https://hydra.iohk.io/jobset/Cardano/cardano-wallet#tabs-jobs
#
# To build locally when you do not have access to remote builders for
# either macOS or Linux, change the `supportedSystems` argument.
# - To build on Linux (without macOS):
#     nix-build --arg supportedSystems '["x86_64-linux"]' release.nix
# - To build on macOS (without Linux):
#     nix-build --arg supportedSystems '["x86_64-darwin"]' supportedCrossSystems '["x86_64-darwin"]' release.nix
#
############################################################################

# The project sources
{ cardano-wallet ? { outPath = ./.; }

  # The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

  # A Hydra option
, scrubJobs ? true

  # Dependencies overrides
, override-inputs ? { }

  # GitHub PR number (as a string), provided as a Hydra input
, pr ? null

  # Can be "staging" or "trying" to indicate that this is a bors jobset
, borsBuild ? null

  # Platform filter string for jobset.
, platform ? "all"

  # Enable debug tracing
, debug ? false
}@args:

(import ./nix/flake-compat.nix (args) // {
  gitrev = cardano-wallet.rev or null;
  src = cardano-wallet;
}).defaultNix.hydraJobs
