{
  pkgs = hackage:
    {
      packages = {
        "HTTP".revision = (((hackage."HTTP")."4000.3.16").revisions).default;
        "HTTP".flags.warp-tests = false;
        "HTTP".flags.network-uri = true;
        "HTTP".flags.conduit10 = false;
        "HTTP".flags.warn-as-error = false;
        "HTTP".flags.mtl1 = false;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "regex-posix".revision = (((hackage."regex-posix")."0.96.0.1").revisions).default;
        "regex-posix".flags._regex-posix-clib = false;
        "network-uri".revision = (((hackage."network-uri")."2.6.4.1").revisions).default;
        "text".revision = (((hackage."text")."1.2.4.1").revisions).default;
        "ed25519".revision = (((hackage."ed25519")."0.0.5.0").revisions).default;
        "ed25519".flags.test-hlint = true;
        "ed25519".flags.test-properties = true;
        "ed25519".flags.no-donna = true;
        "ed25519".flags.test-doctests = true;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "base64-bytestring".revision = (((hackage."base64-bytestring")."1.2.1.0").revisions).default;
        "network".revision = (((hackage."network")."3.1.2.7").revisions).default;
        "network".flags.devel = false;
        "resolv".revision = (((hackage."resolv")."0.1.2.0").revisions).default;
        "Cabal-syntax".revision = (((hackage."Cabal-syntax")."3.6.0.0").revisions).default;
        "echo".revision = (((hackage."echo")."0.1.4").revisions).default;
        "echo".flags.example = false;
        "Cabal".revision = (((hackage."Cabal")."3.4.1.0").revisions).default;
        "Cabal".flags.bundled-binary-generic = false;
        "cryptohash-sha256".revision = (((hackage."cryptohash-sha256")."0.11.102.1").revisions).default;
        "cryptohash-sha256".flags.exe = false;
        "cryptohash-sha256".flags.use-cbits = true;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.14.0").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.12.0").revisions).default;
        "lukko".revision = (((hackage."lukko")."0.1.1.3").revisions).default;
        "lukko".flags.ofd-locking = true;
        "zlib".revision = (((hackage."zlib")."0.6.2.3").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.bundled-c-zlib = false;
        "zlib".flags.pkg-config = false;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.4").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "tar".revision = (((hackage."tar")."0.5.1.1").revisions).default;
        "tar".flags.old-bytestring = false;
        "tar".flags.old-time = false;
        "th-compat".revision = (((hackage."th-compat")."0.1.3").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.1").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.7").revisions).default;
        "base".revision = (((hackage."base")."4.14.3.0").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "async".revision = (((hackage."async")."2.2.4").revisions).default;
        "async".flags.bench = false;
        "random".revision = (((hackage."random")."1.2.1").revisions).default;
        "hackage-security".revision = (((hackage."hackage-security")."0.6.2.1").revisions).default;
        "hackage-security".flags.cabal-syntax = false;
        "hackage-security".flags.base48 = true;
        "hackage-security".flags.lukko = true;
        "hackage-security".flags.use-network-uri = true;
        "hackage-security".flags.old-directory = false;
        "hackage-security".flags.mtl21 = false;
        "process".revision = (((hackage."process")."1.6.13.2").revisions).default;
        "base16-bytestring".revision = (((hackage."base16-bytestring")."0.1.1.7").revisions).default;
        "regex-base".revision = (((hackage."regex-base")."0.94.0.2").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.8").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "rts".revision = (((hackage."rts")."1.0.1").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.5.0").revisions).default;
        "hashable".flags.random-initial-seed = false;
        "hashable".flags.integer-gmp = true;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "edit-distance".revision = (((hackage."edit-distance")."0.2.2.1").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.5.1").revisions).default;
        };
      compiler = {
        version = "8.10.7";
        nix-name = "ghc8107";
        packages = {
          "pretty" = "1.1.3.6";
          "text" = "1.2.4.1";
          "array" = "0.5.4.0";
          "mtl" = "2.2.2";
          "parsec" = "3.1.14.0";
          "bytestring" = "0.10.12.0";
          "filepath" = "1.4.2.1";
          "stm" = "2.5.0.1";
          "ghc-prim" = "0.6.1";
          "ghc-boot-th" = "8.10.7";
          "base" = "4.14.3.0";
          "time" = "1.9.3";
          "process" = "1.6.13.2";
          "directory" = "1.3.6.0";
          "rts" = "1.0.1";
          "transformers" = "0.5.6.2";
          "template-haskell" = "2.16.0.0";
          "deepseq" = "1.4.4.0";
          "unix" = "2.7.2.2";
          "integer-gmp" = "1.0.3.0";
          "binary" = "0.8.8.0";
          "containers" = "0.6.5.1";
          };
        };
      };
  extras = hackage:
    { packages = { cabal-install = ./.plan.nix/cabal-install.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "cabal-install" = {
            flags = {
              "debug-conflict-sets" = lib.mkOverride 900 false;
              "lukko" = lib.mkOverride 900 true;
              "debug-expensive-assertions" = lib.mkOverride 900 false;
              "debug-tracetree" = lib.mkOverride 900 false;
              "native-dns" = lib.mkOverride 900 true;
              };
            };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "Cabal-syntax".components.library.planned = lib.mkOverride 900 true;
          "base16-bytestring".components.library.planned = lib.mkOverride 900 true;
          "echo".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "ed25519".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "Cabal".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "zlib".components.library.planned = lib.mkOverride 900 true;
          "cryptohash-sha256".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "hsc2hs".components.exes."hsc2hs".planned = lib.mkOverride 900 true;
          "resolv".components.library.planned = lib.mkOverride 900 true;
          "edit-distance".components.library.planned = lib.mkOverride 900 true;
          "regex-base".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "network-uri".components.library.planned = lib.mkOverride 900 true;
          "regex-posix".components.library.planned = lib.mkOverride 900 true;
          "HTTP".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "async".components.library.planned = lib.mkOverride 900 true;
          "th-compat".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "tar".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "hackage-security".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "lukko".components.library.planned = lib.mkOverride 900 true;
          "base64-bytestring".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "cabal-install".components.exes."cabal".planned = lib.mkOverride 900 true;
          };
        })
    ];
  }