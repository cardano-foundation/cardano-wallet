############################################################################
# Windows per-test-exe bundle
#
# Creates a flat directory containing one test executable, its DLLs,
# and optional extras (e.g. cardano-cli, golden test data).
#
############################################################################
{
  pkgs,
  test,
  name,
  extraPkgs ? [],
  testDataDirs ? [],
}:
pkgs.runCommand "win-test-${name}" {} ''
  mkdir -p $out
  cp -RL ${test}/bin/* $out/
  ${pkgs.lib.concatMapStringsSep "\n" (pkg: ''
    cp -RLnf ${pkg}/bin/*.dll $out/ 2>/dev/null || true
    cp -RLnf ${pkg}/bin/*.exe $out/ 2>/dev/null || true
  '') extraPkgs}
  ${pkgs.lib.concatMapStringsSep "\n" (dir: ''
    mkdir -p $out/test/data
    cp -RL --no-preserve=mode ${dir}/* $out/test/data/
  '') testDataDirs}
''
