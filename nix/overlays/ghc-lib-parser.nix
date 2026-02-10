# Patch ghc-lib-parser genSym.c: replace atomic_inc64 with atomic_inc.
# Upstream bug in 9.8.5.20250214, fix from:
# https://github.com/digital-asset/ghc-lib/pull/571
final: prev:
let
  patchGenSym = drv:
    drv.overrideAttrs (old: {
      postPatch = (old.postPatch or "") + ''
        if [ -f compiler/cbits/genSym.c ] \
            && grep -q 'atomic_inc64' compiler/cbits/genSym.c; then
          substituteInPlace compiler/cbits/genSym.c \
            --replace-fail 'atomic_inc64' 'atomic_inc'
        fi
      '';
    });
in
{
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (hfinal: hprev: {
        ghc-lib-parser = patchGenSym hprev.ghc-lib-parser;
      });
  });
}
