{ pkgs
, cardano-wallet-jormungandr
, project
, jpkgs ? import ./jormungandr.nix { inherit pkgs; }
, jormungandr-win64 ? jpkgs.jormungandr-win64
, jormungandrLib
}:

let
  testData = ../lib/jormungandr/test/data/jormungandr;
  jormungandrConfig = builtins.toFile "config.yaml" (builtins.toJSON jormungandrLib.defaultJormungandrConfig);
  name = "cardano-wallet-jormungandr-${project.version}-win64";
  jm-bat = pkgs.writeText "jm.bat" ''
    jormungandr.exe --config config.yaml --genesis-block-hash ${jormungandrLib.genesisHash}
  '';
  cw-bat = pkgs.writeText "cw.bat" ''
    cardano-wallet-jormungandr.exe serve --node-port 8081 --genesis-hash ${jormungandrLib.genesisHash}
  '';
in pkgs.runCommand name {
  nativeBuildInputs = [ pkgs.zip pkgs.jq pkgs.gnused project.jormungandr-cli ];
} ''
  mkdir -pv jm $out/nix-support
  cd jm

  cp -v ${cardano-wallet-jormungandr}/bin/* .
  cp -v ${jormungandr-win64}/bin/* .
  cp -v ${jm-bat} jm.bat
  cp -v ${pkgs.libffi}/bin/libffi-6.dll .
  cp -v ${pkgs.openssl.out}/lib/libeay32.dll .
  cp ${jormungandrConfig} config.yaml
  chmod -R +w .

  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
