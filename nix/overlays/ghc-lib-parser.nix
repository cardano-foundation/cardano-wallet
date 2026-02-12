# Patch ghc-lib-parser genSym.c: replace atomic_inc64 with atomic_inc.
# Upstream bug in 9.8.5.20250214, fix from:
# https://github.com/digital-asset/ghc-lib/pull/571
#
# NOTE: This overlay only affects the top-level pkgs.haskellPackages.
# Dev shell tools that depend on ghc-lib-parser (fourmolu, hlint,
# stylish-haskell) are sourced from pkgs.haskellPackages in
# nix/haskell.nix rather than from buildPackages.buildPackages,
# because overlays don't propagate through buildPackages chains.
final: prev: {
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions
      (prev.haskell.packageOverrides or (_: _: {}))
      (hfinal: hprev: {
        ghc-lib-parser = final.haskell.lib.compose.overrideCabal (drv: {
          postPatch = (drv.postPatch or "") + ''
            if [ -f compiler/cbits/genSym.c ] \
                && grep -q 'atomic_inc64' compiler/cbits/genSym.c; then
              substituteInPlace compiler/cbits/genSym.c \
                --replace-fail 'atomic_inc64' 'atomic_inc'
            fi
          '';
        }) hprev.ghc-lib-parser;
      });
  };
}
