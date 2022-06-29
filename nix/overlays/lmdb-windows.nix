# Taken from <https://github.com/input-output-hk/ouroboros-network/commit/8343fa31fca7fc5c32f01a9d98861c71b4dbcfa7#diff-4e3bd328e4c998f53b19da7639df45b2db63601be4b4343534880adfbeaa5840R43>.

# for LMDB cross compilation
# remove once our nixpkgs pin contains https://github.com/NixOS/nixpkgs/pull/171686
self: super:

super.lib.optionalAttrs super.stdenv.hostPlatform.isWindows {
  lmdb = super.lmdb.overrideAttrs (oldAttrs: {
    makeFlags = oldAttrs.makeFlags ++ [ "SOEXT=.dll" "BINEXT=.exe" ];
    buildInputs = [ super.windows.pthreads ];
    patches = [ ./lmdb-mingw.patch ];
  });
}
