{ pkgs ? import <nixpkgs> {} }:

let
  cardanoSrc = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-sl";
    rev = "59afcfcd51b89184328f45a1f1adeed15c32d17d";
    sha256 = "0frkhm5krwy04rdhyh25mr2j7zms14jfzlsa611gymh2r7nlwl15";
  };
  cardanoPkgs = import cardanoSrc { gitrev = cardanoSrc.rev; };
in
  cardanoPkgs.cardano-sl-node-static
