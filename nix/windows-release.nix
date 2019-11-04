############################################################################
# Windows release CARDAN~1.ZIP
#
# This bundles up the windows build and its dependencies, adds an
# example self-node configuration, and some .BAT files for launching,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, project
, cardano-wallet-jormungandr
, tests ? []
, benchmarks ? []
}:

let
  testData = ../lib/jormungandr/test/data/jormungandr;
  name = "cardano-wallet-jormungandr-${project.version}-win64";
  jm-bat = pkgs.writeText "jm.bat" ''
    jormungandr.exe --config config.yaml --genesis-block block0.bin --secret secret.yaml
  '';
  cw-bat = pkgs.writeText "cw.bat" ''
    cardano-wallet-jormungandr.exe serve --node-port 8080 --genesis-block-hash HASH --database c:\\cardano-wallet-jormungandr\\wallets
  '';
  launch-bat = pkgs.writeText "launch.bat" ''
    cardano-wallet-jormungandr.exe launch --genesis-block-hash HASH --state-dir c:\\cardano-wallet-jormungandr %*
  '';

in pkgs.runCommand name {
  nativeBuildInputs = [ pkgs.zip pkgs.jq pkgs.gnused project.jormungandr-cli ];
  passthru = { inherit tests benchmarks; };
} ''
  mkdir -pv jm $out/nix-support
  cd jm

  cp -v ${cardano-wallet-jormungandr}/bin/* .
  cp -v ${testData}/block0.bin ${testData}/secret.yaml .
  cp -v ${jm-bat} jm.bat
  hash="$(jcli genesis hash --input block0.bin)"
  sed -e "s/HASH/$hash/" ${cw-bat} > cw.bat
  sed -e "s/HASH/$hash/" ${launch-bat} > launch.bat
  sed -e 's/storage:.*/storage: "c:\\\\cardano-wallet-jormungandr\\\\chain"/' \
      ${testData}/config.yaml > config.yaml

  ${pkgs.lib.concatMapStringsSep "\n" (test: ''
    pkg=`ls -1 ${test}`
    exe=`cd ${test}; ls -1 $pkg`
    name=$pkg-test-$exe
    cp ${test}/$pkg/$exe $name
    echo $name >> tests.bat
    echo "if %errorlevel% neq 0 exit /b %errorlevel%" >> tests.bat
  '') tests}

  chmod -R +w .

  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
