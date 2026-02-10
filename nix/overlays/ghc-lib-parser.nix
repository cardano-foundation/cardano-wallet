# Patch ghc-lib-parser genSym.c: replace atomic_inc64 with atomic_inc.
# Upstream bug in 9.8.5.20250214, fix from:
# https://github.com/digital-asset/ghc-lib/pull/571
final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev: {
      ghc-lib-parser = hprev.ghc-lib-parser.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          substituteInPlace compiler/cbits/genSym.c \
            --replace-fail 'atomic_inc64' 'atomic_inc'
        '';
      });
    };
  };
}
