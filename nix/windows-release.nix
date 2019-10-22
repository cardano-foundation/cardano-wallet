############################################################################
# Windows release CARDAN~1.ZIP
#
# This bundles up the windows build and its dependencies, adds an
# example self-node configuration, and some .BAT files for launching,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, cardano-wallet-jormungandr
, project
}:

let
  testData = ../lib/jormungandr/test/data/jormungandr;
  name = "cardano-wallet-jormungandr-${project.version}-win64";
  jm-bat = pkgs.writeText "jm.bat" ''
    jormungandr.exe --config config.yaml --genesis-block block0.bin --secret secret.yaml
  '';
  cw-bat = pkgs.writeText "cw.bat" ''
    cardano-wallet-jormungandr.exe serve --node-port 8081 --genesis-hash HASH --database c:\\cardano-wallet-jormungandr\\wallet.db
  '';
in pkgs.runCommand name {
  nativeBuildInputs = [ pkgs.zip pkgs.jq pkgs.gnused project.jormungandr-cli ];
} ''
  mkdir -pv jm $out/nix-support
  cd jm

  cp -v ${cardano-wallet-jormungandr}/bin/* .
  cp -v ${testData}/block0.bin ${testData}/secret.yaml .
  cp -v ${jm-bat} jm.bat
  hash="$(jcli genesis hash --input block0.bin)"
  sed -e "s/HASH/$hash/" ${cw-bat} > cw.bat
  sed -e 's/storage:.*/storage: "c:\\\\cardano-wallet-jormungandr\\\\storage"/' \
      ${testData}/config.yaml > config.yaml
  chmod -R +w .

  zip -r $out/${name}.zip .
  echo "file binary-dist $out/${name}.zip" > $out/nix-support/hydra-build-products
''
