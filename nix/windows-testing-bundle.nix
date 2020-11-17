############################################################################
# Windows testing bundle
#
# This bundles up the windows build and its dependencies, adds an
# example self-node configuration, and some .BAT files for launching,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, project
, cardano-wallet
, cardano-node
, cardano-cli
, tests ? []
, benchmarks ? []
}:

let
  testData = {
    core = ../lib/core/test/data;
    shelley = ../lib/shelley/test/data;
  };

  name = "cardano-wallet-${project.version}-tests-win64";

in pkgs.runCommand name {
  nativeBuildInputs = [ pkgs.zip pkgs.gnused ];
  passthru = { inherit tests benchmarks; };
} ''
  mkdir -pv jm jm/test/data jm/test/integration $out/nix-support
  cd jm

  # Copy in wallet and node EXEs and DLLs.
  for pkg in ${cardano-wallet} ${cardano-cli}; do
    cp -vf $pkg/bin/* .
  done

  # Copy test data to location expected by test suites.
  cp -Rv --no-preserve=mode ${testData.core}/* ${testData.shelley}/* test/data

  # Copy in test executables and rename.
  # Add each one to tests.bat.
  ${pkgs.lib.concatMapStringsSep "\n" (test: ''
    exe=`cd ${test}/bin; ls -1 *.exe`
    name=${test.packageName}-test-$exe
    cp ${test}/bin/$exe $name
    echo $name >> tests.bat
    echo "if %errorlevel% neq 0 exit /b %errorlevel%" >> tests.bat
  '') tests}

  # Copy in benchmark executables and rename.
  ${pkgs.lib.concatMapStringsSep "\n" (bench: ''
    exe=`cd ${bench}/bin; ls -1 *.exe`
    name=${bench.packageName}-bench-$exe
    cp ${bench}/bin/$exe $name
  '') benchmarks}

  chmod -R +w .

  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
