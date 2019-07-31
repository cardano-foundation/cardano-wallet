{ rustPlatform
, fetchFromGitHub
, protobuf
, stdenv
, darwin
,  ... }:


let
  Security = darwin.apple_sdk.frameworks.Security;
in rustPlatform.buildRustPackage rec {
  name = "cardano-http-bridge-${version}";

  version = "0.0.5";
  src = fetchFromGitHub {
    owner = "rvl";
    repo = "cardano-http-bridge";
    fetchSubmodules = true;
    rev = "5cd4ef1f6c0622168a108c82f312a26582f163ad";
    sha256 = "0a1l9c3i8xcw4fql76j6azgnqiip7hsg7a1jmbhgnlybn079ym9p";
  };
  cargoSha256 = "0dmd1v3pqkhjrnqy64ldpg1vhj01m95gm57njcfihiscwy3nwj70";

  buildInputs = [ protobuf ] ++ stdenv.lib.optional stdenv.isDarwin Security;

  PROTOC = "${protobuf}/bin/protoc";

  # workaround https://github.com/NixOS/nixpkgs/issues/61618
  preConfigure = ''
    export HOME=`mktemp -d`
  '';
}
