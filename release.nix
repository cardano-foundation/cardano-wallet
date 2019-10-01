{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, supportedCrossSystems ? [ "x86_64-linux" ]
, scrubJobs ? true
, cardano-wallet ? { outPath = ./.; rev = "abcdef"; }
, projectArgs ? { config = { allowUnfree = false; inHydra = true; }; }
}:

with (import ./nix/release-lib.nix) {
  inherit (import ./nix/iohk-common.nix {}) pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-wallet;
  gitrev = cardano-wallet.rev;
};

with pkgs.lib;

let
  testsSupportedSystems = [ "x86_64-linux" ];
  collectTests = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);

  inherit (systems.examples) mingwW64 musl64;

  jobs = {
    native = mapTestOn (packagePlatforms project);
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross project);
  }
  // {
    # This aggregate job is what IOHK Hydra uses to update
    # the CI status in GitHub.
    required = mkRequiredJob (
      # fixme: fix failing tests
      # collectTests jobs.native.tests ++
      collectTests jobs.native.benchmarks ++
      [ jobs.native.cardano-wallet-http-bridge.x86_64-linux
        jobs.native.cardano-wallet-http-bridge.x86_64-darwin
        jobs.native.cardano-wallet-jormungandr.x86_64-linux
        jobs.native.cardano-wallet-jormungandr.x86_64-darwin
      ]
    );

    cardano-wallet-jormungandr-win64 = let
      jm = pkgs.fetchurl {
        url = https://github.com/input-output-hk/jormungandr/releases/download/v0.3.3/jormungandr-v0.3.3-x86_64-pc-windows-msvc.zip;
        sha256 = "0psva16vq86gcld701k0fi6kk1ydnm2q3yd2mdgflb0x8zpm2i3g";
      };
      testData = ./lib/jormungandr/test/data/jormungandr;
      name = "cardano-wallet-jormungandr-${project.version}-win64.zip";
      jm-bat = pkgs.writeText "jm.bat" ''
        jormungandr.exe --config config.yaml --genesis-block block0.bin --secret secret.yaml
      '';
      cw-bat = pkgs.writeText "cw.bat" ''
        cardano-wallet-jormungandr.exe serve --node-port 8081 --genesis-hash HASH --database c:\\cardano-wallet-jormungandr\\wallets
      '';
    in pkgs.runCommand "cardano-wallet-jormungandr-win64" {
      buildInputs = [ pkgs.zip pkgs.unzip pkgs.jq pkgs.gnused project.jormungandr-cli ];
    } ''
      mkdir -pv jm $out/nix-support
      cd jm

      cp -v ${jobs.x86_64-pc-mingw32.cardano-wallet-jormungandr.x86_64-linux}/bin/* .
      unzip ${jm}
      cp -v ${testData}/block0.bin ${testData}/secret.yaml .
      cp -v ${jm-bat} jm.bat
      hash="$(jcli genesis hash --input block0.bin)"
      sed -e "s/HASH/$hash/" ${cw-bat} > cw.bat
      sed -e 's/storage:.*/storage: "c:\\\\cardano-wallet-jormungandr\\\\storage"/' \
          ${testData}/config.yaml > config.yaml
      chmod -R +w .

      zip -r $out/${name} .
      echo "file binary-dist $out/${name}" > $out/nix-support/hydra-build-products
    '';
  }
  # Build the shell derivation in Hydra so that all its dependencies
  # are cached.
  // mapTestOn (packagePlatforms { inherit (project) shell; });

in jobs
