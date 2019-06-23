{ rustPlatform
, fetchFromGitHub
, protobuf
,  ... }:


rustPlatform.buildRustPackage rec {
  name = "cardano-http-bridge-${version}";

  version = "0.0.3";
  src = fetchFromGitHub {
    owner = "KtorZ";
    repo = "cardano-http-bridge";
    fetchSubmodules = true;
    rev = "a0e05390bee29d90daeec958fdce97e08c437143";
    sha256 = "1ix9b0pp50397g46h9k8axyrh8395a5l7zixsqrsyq90jwkbafa3";
  };
  cargoSha256 = "1phij6gcs70rsv1y0ac6lciq384g2f014mn15pjvd02l09nx7k49";

  buildInputs = [ protobuf ];

  PROTOC = "${protobuf}/bin/protoc";

}
