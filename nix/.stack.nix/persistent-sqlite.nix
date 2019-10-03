{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {
      systemlib = false;
      use-pkgconfig = false;
      build-sanity-exe = false;
      full-text-search = true;
      uri-filenames = true;
      have-usleep = true;
      json1 = true;
      use-stat3 = false;
      use-stat4 = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "persistent-sqlite"; version = "2.10.5"; };
      license = "MIT";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://www.yesodweb.com/book/persistent";
      url = "";
      synopsis = "Backend for the persistent library using sqlite3.";
      description = "This package includes a thin sqlite3 wrapper based on the direct-sqlite package, as well as the entire C library, so there are no system dependencies.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.persistent)
          (hsPkgs.aeson)
          (hsPkgs.bytestring)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.microlens-th)
          (hsPkgs.monad-logger)
          (hsPkgs.resource-pool)
          (hsPkgs.resourcet)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.unliftio-core)
          (hsPkgs.unordered-containers)
          ];
        libs = (pkgs.lib).optionals (flags.systemlib) ((pkgs.lib).optional (!flags.use-pkgconfig) (pkgs."sqlite3")) ++ (pkgs.lib).optional (!system.isWindows) (pkgs."pthread");
        pkgconfig = (pkgs.lib).optionals (flags.systemlib) ((pkgs.lib).optional (flags.use-pkgconfig) (pkgconfPkgs."sqlite3"));
        };
      exes = {
        "sanity" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.persistent-sqlite)
            (hsPkgs.monad-logger)
            ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.persistent)
            (hsPkgs.persistent-sqlite)
            (hsPkgs.persistent-template)
            (hsPkgs.persistent-test)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.exceptions)
            (hsPkgs.fast-logger)
            (hsPkgs.hspec)
            (hsPkgs.HUnit)
            (hsPkgs.monad-logger)
            (hsPkgs.QuickCheck)
            (hsPkgs.resourcet)
            (hsPkgs.system-fileio)
            (hsPkgs.system-filepath)
            (hsPkgs.temporary)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.time)
            (hsPkgs.unliftio-core)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/KtorZ/persistent";
      rev = "79f2ece07eafae005a703c8eda1bd2420b5e07b5";
      sha256 = "081bhdg52wn7vgxsgl4aimy73ccai05j64r24hwkdnjj4kz96lia";
      });
    postUnpack = "sourceRoot+=/persistent-sqlite; echo source root reset to \$sourceRoot";
    }