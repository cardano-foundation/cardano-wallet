{ pkgs ? (import ./nix/iohk-common.nix).pkgs
}:

with pkgs;

rustPlatform.buildRustPackage {
  name = "cardano-http-bridge";

  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-http-bridge";
    fetchSubmodules = true;
    rev = "b81c805e0fe62e6b8eacdc11fee573fa5668cbc8";
    sha256 = "0vkg1hgcg9xqwzi9a1rap12sgyvc7ppnf89rqlf8pgsz3akzr4qd";
  };

  buildInputs = [ sqlite protobuf rustfmt ];

  PROTOC = "${protobuf}/bin/protoc";

  cargoSha256 = "19g5fy8af65vd9rl66058c67nlrz7r6mjd0cy83865d7q81hdl8r";
}
