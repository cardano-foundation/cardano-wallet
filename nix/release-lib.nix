########################################################################
# This is a modified version of Nixpkgs release-lib.
#
# 1. The attrset of project derivations is now called "project" not
#    instead of "pkgs".
#
# 2. The arguments to pass to the project are now called "projectArgs"
#    instead of "nixpkgsArgs".
#
# 3. The Nixpkgs package set is available as "pkgs".
#
# 4. The "packagePlatforms" function ignores meta.platforms and just
#    uses the given supportedSystems. Old function available as
#    "packagePlatformsOrig".
#
# 5. There is a new "packagePlatformsCross" and corresponding
#    "supportedCrossSystems" argument.
#
# 6. Introduces a "mkRequiredJob" derivation for GitHub CI status
#    reporting.
#
########################################################################

{ pkgs ? import <nixpkgs> {}
  # What systems to use for building.
, supportedSystems
  # What systems to use for cross-compilation builds.
, supportedCrossSystems ? [ "x86_64-linux" ]
  # Hydra option
, scrubJobs ? true
  # Your project's default.nix
, packageSet
  # Arguments to pass to packageSet.
, projectArgs ? { config = { allowUnfree = false; inHydra = true; }; }
  # Project source git revision, for "mkRequiredJob"
, gitrev ? null
}:

with pkgs.lib;

let
  release-lib = import (pkgs.path + "/pkgs/top-level/release-lib.nix") {
    inherit supportedSystems scrubJobs packageSet;
     nixpkgsArgs = projectArgs;
  };

in
  release-lib // rec {
    # The Nixpkgs package set, passed through for convenience.
    inherit pkgs;

    # Your project's package set.
    project = release-lib.pkgs;

    # Unlike the release-lib version, this ignores meta.platforms in
    # the derivations.
    packagePlatforms = mapAttrs (name: value:
      let res = builtins.tryEval (
        if isDerivation value then
          supportedSystems
        else if value.recurseForDerivations or false || value.recurseForRelease or false then
          packagePlatforms value
        else
          []);
      in if res.success then res.value else []
      );
    packagePlatformsOrig = release-lib.packagePlatforms;

    # A version of packagePlatforms that uses supportedCrossSystems
    packagePlatformsCross = mapAttrs (name: value:
      let res = builtins.tryEval (
        if isDerivation value then
          value.meta.hydraPlatforms
            or (supportedMatchesCross (value.meta.platforms or [ "x86_64-linux" ]))
        else if value.recurseForDerivations or false || value.recurseForRelease or false then
          packagePlatformsCross value
        else
          []);
      in if res.success then res.value else []
      );

    # Given a list of 'meta.platforms'-style patterns, return the sublist of
    # `supportedCrossSystems` containing systems that matches at least one of the given
    # patterns.
    #
    # This is written in a funny way so that we only elaborate the systems once.
    supportedMatchesCross = let
        supportedPlatforms = map
          (system: systems.elaborate { inherit system; })
          supportedCrossSystems;
      in metaPatterns: let
        anyMatch = platform:
          any (meta.platformMatch platform) metaPatterns;
        matchingPlatforms = filter anyMatch supportedPlatforms;
      in map ({ system, ...}: system) matchingPlatforms;

    # Creates a single aggregate job for the purpose of updating the
    # CI status in GitHub from IOHK Hydra.
    mkRequiredJob = constituents: let
      # This file seems pointless, but it forces Hydra to re-evaluate
      # every commit. The side-effect of that is that Hydra reports build
      # status to GitHub for every commit, which we want, and it wouldn't
      # normally do.
      build-version = pkgs.writeText "version.json" (builtins.toJSON
        (filterAttrs (n: _: n == "version") project // { inherit gitrev; }));
    in pkgs.releaseTools.aggregate {
      name = "github-required";
      meta.description = "All jobs required to pass CI";
      constituents = constituents ++ [ build-version];
    };
  }
