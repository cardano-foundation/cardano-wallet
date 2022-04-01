############################################################################
# Release package
#
# This bundles up the build of the given exes, with their
# dependencies, and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, platform
, exes
, format
}:

let
  inherit (pkgs) lib cardanoWalletLib;

  exe = assert lib.assertMsg (lib.length exes > 0) "empty list of exes";
    lib.head exes;
  name = "${cardanoWalletLib.versionTag exe.meta.name}-${platform}";

  makeTarball = format == "tar.gz";
  makeZip = format == "zip";

  isLinux = platform == "linux64";
  isMacOS = platform == "macos-intel" || platform == "macos-silicon";
  isWindows = platform == "win64";

in
  assert lib.assertMsg (makeTarball || makeZip)
    "format must be tar.gz or zip";
  assert lib.assertMsg (isLinux || isMacOS || isWindows)
    "wrong platform \"${platform}\"";

pkgs.stdenv.mkDerivation {
  inherit name;
  buildInputs = with pkgs.buildPackages; [
    iohk-nix-utils
    nix
  ]
  ++ (if pkgs.stdenv.hostPlatform.isDarwin then [ darwin.binutils ] else [ binutils ])
  ++ lib.optionals makeTarball [ gnutar gzip ]
  ++ lib.optionals makeZip [ zip ];
  checkInputs = with pkgs.buildPackages; [
    ruby
    gnugrep
    gnused
  ]
  ++ lib.optionals isMacOS [ darwin.cctools ]
  ++ lib.optionals isWindows [ unzip wineMinimal ]
  ++ lib.optional (stdenv.buildPlatform.libc == "glibc") glibcLocales;

  doCheck = true;
  phases = [ "buildPhase" "checkPhase" ];
  pkgname = "${name}.${format}";
  exeName = lib.getName exe.name;
  buildPhase = ''
    mkdir -p $out/nix-support $name
    cp --no-preserve=timestamps --no-clobber --recursive \
      ${lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} \
      $name
    chmod -R +w $name

  '' + lib.optionalString isMacOS ''
    # Rewrite library paths to standard non-nix locations
    ( cd $name; rewrite-libs . `ls -1 | grep -Fv .dylib`
      for a in *; do /usr/bin/codesign -f -s - $a; done
    )

  '' + lib.optionalString (isLinux || isMacOS) ''
    mkdir -p $name/auto-completion/{bash,zsh,fish}
    cp ${exe}/share/bash-completion/completions/* $name/auto-completion/bash/$exeName.sh
    cp ${exe}/share/zsh/vendor-completions/* $name/auto-completion/zsh/_$exeName
    cp ${exe}/share/fish/vendor_completions.d/* $name/auto-completion/fish/$exeName.fish

  '' + lib.optionalString makeTarball ''
    tar -czf $out/$pkgname $name
  '' + lib.optionalString makeZip ''
    ( cd $name; zip -r $out/$pkgname . )
  '' + ''
    echo "file binary-dist $out/$pkgname" > $out/nix-support/hydra-build-products
  '' + lib.optionalString isWindows ''

    # make a separate configuration package if needed
    if [ -d ${exe}/configuration ]; then
      cp --no-preserve=mode,timestamps -R ${exe}/configuration .

      ( cd configuration; zip -r $out/$name-configuration.zip . )
      echo "file binary-dist $out/$name-configuration.zip" >> $out/nix-support/hydra-build-products
    fi

    # make a separate deployments configuration package if needed
    if [ -d ${exe}/deployments ]; then
      cp --no-preserve=mode,timestamps -R ${exe}/deployments .

      ( cd deployments; zip -r $out/$name-deployments.zip . )
      echo "file binary-dist $out/$name-deployments.zip" >> $out/nix-support/hydra-build-products
    fi
  '';

  # test that executables work
  exeRunner = lib.optionalString isWindows "wine64";
  checkPhase = ''
    cd `mktemp -d`
    echo " - extracting $pkgname"
    ${lib.optionalString makeTarball "tar -xzvf $out/$pkgname"}
    ${lib.optionalString makeZip "unzip $out/$pkgname"}

  '' + lib.optionalString isWindows ''
    # setup wine
    export WINEPREFIX=$TMP
    export HOME=$TMP
    export WINEDLLOVERRIDES="winemac.drv=d"
    export WINEDEBUG=warn-all,fixme-all,-menubuilder,-mscoree,-ole,-secur32,-winediag

  '' + ''
    export PATH=`pwd`/$name:$PATH

    echo " - running checks"
    ruby ${../scripts/check-bundle.rb} $exeName $exeRunner
  '';
} // lib.optionalAttrs (pkgs.stdenv.buildPlatform.libc == "glibc") {
  LOCALE_ARCHIVE = "${pkgs.buildPackages.glibcLocales}/lib/locale/locale-archive";
  LANG = "en_US.UTF-8";
  LC_ALL = "en_US.UTF-8";
}
