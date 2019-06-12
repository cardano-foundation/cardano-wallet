{ rustPlatform
, fetchFromGitHub
, protobuf
,  ... }:


rustPlatform.buildRustPackage rec {
  name = "cardano-http-bridge-${version}";

  version = "0.0.2";
  src = fetchFromGitHub {
    owner = "rvl";
    repo = "cardano-http-bridge";
    fetchSubmodules = true;
    rev = "ba3e172b90f3b9ebabe1bcca4c71183c3118ebe8";
    sha256 = "1dr920bx48agj83h5cn1jx9nygkb5c23qi58brvfar6aivsbvx4c";
  };
  cargoSha256 = "0l6z1rsb8hw36w7kg6ms4l688klz17cw36q01pdb96ayhcacys49";

  buildInputs = [ protobuf ];

  PROTOC = "${protobuf}/bin/protoc";

}
