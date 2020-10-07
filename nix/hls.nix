{ haskell-nix, fetchFromGitHub, compiler-nix-name }:

let
  hlsPkgs = haskell-nix.cabalProject {
    src = fetchFromGitHub {
      name = "haskell-language-server";
      owner = "haskell";
      repo = "haskell-language-server";
      rev = "0.5.0";
      sha256 = "0vkh5ff6l5wr4450xmbki3cfhlwf041fjaalnwmj7zskd72s9p7p";
      fetchSubmodules = true;
    };

    # Fix source info of brittany dep
    lookupSha256 = { location, tag, ... } : {
      "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
      }."${location}"."${tag}";

    # Use same GHC as the project
    inherit compiler-nix-name;

    # # Materialization voodoo (disabled for now).
    # inherit index-state checkMaterialization;
    # Invalidate and update if you change the version
    # plan-sha256 = "144p19wpydc6c56f0zw5b7c17151n0cghimr9wd8rlhifymmky2h";
  };

in
  {
    inherit (hlsPkgs.haskell-language-server.components.exes) haskell-language-server;
    inherit (hlsPkgs.hie-bios.components.exes) hie-bios;
  }
