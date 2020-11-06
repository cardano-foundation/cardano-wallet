{
  pkgs = hackage:
    {
      packages = {
        "cryptohash-sha256".revision = (((hackage."cryptohash-sha256")."0.11.101.0").revisions).default;
        "cryptohash-sha256".flags.exe = false;
        "binary".revision = (((hackage."binary")."0.8.6.0").revisions).default;
        "tar".revision = (((hackage."tar")."0.5.1.1").revisions).default;
        "tar".flags.old-time = false;
        "tar".flags.old-bytestring = false;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "edit-distance".revision = (((hackage."edit-distance")."0.2.2.1").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "network-uri".revision = (((hackage."network-uri")."2.6.3.0").revisions).default;
        "zlib".revision = (((hackage."zlib")."0.6.2.2").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.bundled-c-zlib = false;
        "zlib".flags.pkg-config = false;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "random".revision = (((hackage."random")."1.1").revisions).default;
        "network".revision = (((hackage."network")."3.1.2.0").revisions).default;
        "network".flags.devel = false;
        "async".revision = (((hackage."async")."2.2.2").revisions).default;
        "async".flags.bench = false;
        "parsec".revision = (((hackage."parsec")."3.1.13.0").revisions).default;
        "echo".revision = (((hackage."echo")."0.1.3").revisions).default;
        "echo".flags.example = false;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.7").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "resolv".revision = (((hackage."resolv")."0.1.2.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.3.0").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.14.0.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.0.1").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.8.2").revisions).default;
        "text".revision = (((hackage."text")."1.2.3.1").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."3.2.0.0").revisions).default;
        "Cabal".flags.bundled-binary-generic = false;
        "base64-bytestring".revision = (((hackage."base64-bytestring")."1.2.0.0").revisions).default;
        "base".revision = (((hackage."base")."4.12.0.0").revisions).default;
        "time".revision = (((hackage."time")."1.8.0.2").revisions).default;
        "base16-bytestring".revision = (((hackage."base16-bytestring")."0.1.1.7").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.examples = false;
        "HTTP".revision = (((hackage."HTTP")."4000.3.15").revisions).default;
        "HTTP".flags.mtl1 = false;
        "HTTP".flags.conduit10 = false;
        "HTTP".flags.warn-as-error = false;
        "HTTP".flags.warp-tests = false;
        "HTTP".flags.network-uri = true;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "ed25519".revision = (((hackage."ed25519")."0.0.5.0").revisions).default;
        "ed25519".flags.test-hlint = true;
        "ed25519".flags.test-properties = true;
        "ed25519".flags.test-doctests = true;
        "ed25519".flags.no-donna = true;
        "process".revision = (((hackage."process")."1.6.5.0").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "lukko".revision = (((hackage."lukko")."0.1.1.2").revisions).default;
        "lukko".flags.ofd-locking = true;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.6.5").revisions).default;
        "array".revision = (((hackage."array")."0.5.3.0").revisions).default;
        "hackage-security".revision = (((hackage."hackage-security")."0.6.0.1").revisions).default;
        "hackage-security".flags.old-directory = false;
        "hackage-security".flags.use-network-uri = true;
        "hackage-security".flags.base48 = true;
        "hackage-security".flags.lukko = true;
        "hackage-security".flags.mtl21 = false;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.6.5";
        nix-name = "ghc865";
        packages = {
          "binary" = "0.8.6.0";
          "ghc-prim" = "0.5.3";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "parsec" = "3.1.13.0";
          "directory" = "1.3.3.0";
          "template-haskell" = "2.14.0.0";
          "containers" = "0.6.0.1";
          "bytestring" = "0.10.8.2";
          "text" = "1.2.3.1";
          "base" = "4.12.0.0";
          "time" = "1.8.0.2";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "process" = "1.6.5.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.6.5";
          "array" = "0.5.3.0";
          "integer-gmp" = "1.0.2.0";
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
              "native-dns" = lib.mkOverride 900 true;
              "debug-expensive-assertions" = lib.mkOverride 900 false;
              "debug-tracetree" = lib.mkOverride 900 false;
              "lukko" = lib.mkOverride 900 true;
              "debug-conflict-sets" = lib.mkOverride 900 false;
              };
            };
          };
        })
    ];
  }