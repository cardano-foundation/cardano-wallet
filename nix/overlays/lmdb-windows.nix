# See also <<https://github.com/input-output-hk/ouroboros-network/commit/8343fa31fca7fc5c32f01a9d98861c71b4dbcfa7#diff-4e3bd328e4c998f53b19da7639df45b2db63601be4b4343534880adfbeaa5840R43>.
#
# for LMDB cross compilation
# remove once:
# • our nixpkgs pin contains https://github.com/NixOS/nixpkgs/pull/171686
# • and the Windows build uses `--out-implib`
# • and the MacOS build uses the `.dylib` extension
self: super:

let

  nixpkgsUnstable = builtins.fetchTarball {
    # Windows cross compilation fix + pkgconfig <https://github.com/NixOS/nixpkgs/pull/171686>
    url = "https://github.com/NixOS/nixpkgs/archive/c745afadf4f7870fb398cf94ddf51652d6fed2d8.tar.gz";
    sha256 = "0k6l9zq3iqv3qh7jyw2krsvlbr9mm7z6w97r45vl68lzfynmnyrw";
  };

in

{
  lmdb = (self.callPackage "${nixpkgsUnstable}/pkgs/development/libraries/lmdb/default.nix" {}).overrideAttrs (oldAttrs: {
    patches = oldAttrs.patches ++ self.lib.optionals self.stdenv.hostPlatform.isWindows [
      # We need the `liblmdb.dll.a` import library on Windows:
      ./lmdb-mingw-implib.patch  # adapted from <https://github.com/msys2/MINGW-packages/tree/master/mingw-w64-lmdb>
    ];
  });
}
