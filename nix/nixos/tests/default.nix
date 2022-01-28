{ pkgs
, project
}:
let
  importTest = fn: args:
    let
      imported = import fn;
      test = import (pkgs.path + "/nixos/tests/make-test-python.nix") imported;
    in
    test ({
      inherit pkgs project;
      inherit (pkgs) system config;
    } // args);
in
{
  basicTest = importTest ./service-basic-test.nix { };
}
