############################################################################
# Windows testing bundle
#
# This bundles up the windows build and its dependencies, adds an
# example self-node configuration, and some .BAT files for launching,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
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
    cli = ../lib/cli/test/data;
  };

  name = "cardano-wallet-${cardano-wallet.version}-tests-win64";

in pkgs.runCommand name {
  nativeBuildInputs = [ pkgs.zip pkgs.gnused ];
  passthru = { inherit tests benchmarks; };
} ''
  mkdir -pv bundle/test/data $out/nix-support
  cd bundle

  # Copy in wallet and node EXEs and DLLs.
  for pkg in ${cardano-wallet} ${cardano-node} ${cardano-cli}; do
    cp -vf $pkg/bin/* .
  done

  # Copy test data to location expected by test suites.
  ${pkgs.lib.concatMapStringsSep "\n" (dir: ''
  cp -Rv --no-preserve=mode ${dir}/* test/data
  '') (pkgs.lib.attrValues testData)}

  # Copy in test executables and rename.
  # Add each one to tests.bat.
  ${pkgs.lib.concatMapStringsSep "\n" (test: ''
    exe=`cd ${test}/bin; ls -1 *.exe`
    name=${test.passthru.identifier.name}-test-$exe
    cp ${test}/bin/$exe $name
    echo $name >> tests.bat
    echo "if %errorlevel% neq 0 exit /b %errorlevel%" >> tests.bat
  '') tests}

  # Copy in benchmark executables and rename.
  ${pkgs.lib.concatMapStringsSep "\n" (bench: ''
    exe=`cd ${bench}/bin; ls -1 *.exe`
    name=${bench.passthru.identifier.name}-bench-$exe
    cp ${bench}/bin/$exe $name
  '') benchmarks}

  chmod -R +w .

  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
