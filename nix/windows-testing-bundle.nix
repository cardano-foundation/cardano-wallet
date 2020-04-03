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
, cardano-wallet-itn
, cardano-wallet
, cardano-node
, tests ? []
, benchmarks ? []
}:

let
  testData = {
    core = ../lib/core/test/data;
    jormungandr = ../lib/jormungandr/test/data;
    byron = ../lib/byron/test/data;
    daedalusIPC = ../lib/jormungandr/test/integration/js;
  };

  name = "cardano-wallet-${project.version}-tests-win64";
  jm-bat = pkgs.writeText "jm.bat" ''
    jormungandr.exe --config test\data\jormungandr\config.yaml --genesis-block test\data\jormungandr\block0.bin --secret test\data\jormungandr\secret.yaml
  '';
  cw-bat = pkgs.writeText "cw.bat" ''
    cardano-wallet-itn.exe serve --node-port 8080 --genesis-block-hash HASH --database c:\cardano-wallet-itn\wallets
  '';
  launch-bat = pkgs.writeText "launch.bat" ''
    cardano-wallet-itn.exe launch --genesis-block test\data\jormungandr\block0.bin --state-dir c:\cardano-wallet-itn -- --config test\data\jormungandr\config.yaml --secret test\data\jormungandr\secret.yaml
  '';

in pkgs.runCommand name {
  nativeBuildInputs = [ pkgs.zip pkgs.gnused project.jormungandr-cli ];
  passthru = { inherit tests benchmarks; };
} ''
  mkdir -pv jm jm/test/data jm/test/integration $out/nix-support
  cd jm

  cp -vf ${cardano-wallet-itn}/bin/* .
  cp -vf ${cardano-wallet}/bin/* .
  cp -Rv --no-preserve=mode ${testData.core}/* ${testData.jormungandr}/* ${testData.byron}/* test/data
  cp -v ${jm-bat} jm.bat
  hash="$(jcli genesis hash --input test/data/jormungandr/block0.bin)"
  sed -e "s/HASH/$hash/" ${cw-bat} > cw.bat
  sed -e "s/HASH/$hash/" ${launch-bat} > launch.bat
  sed -e 's/storage:.*/storage: "c:\\cardano-wallet-itn\\chain"/' \
      ${testData.jormungandr}/jormungandr/config.yaml > config.yaml

  ${pkgs.lib.concatMapStringsSep "\n" (test: ''
    exe=`cd ${test}/bin; ls -1 *.exe`
    name=${test.packageName}-test-$exe
    cp ${test}/bin/$exe $name
    echo $name >> tests.bat
    echo "if %errorlevel% neq 0 exit /b %errorlevel%" >> tests.bat
  '') tests}

  ${pkgs.lib.concatMapStringsSep "\n" (bench: ''
    exe=`cd ${bench}/bin; ls -1 *.exe`
    name=${bench.packageName}-bench-$exe
    cp ${bench}/bin/$exe $name
  '') benchmarks}

  chmod -R +w .

  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
