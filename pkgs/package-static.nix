{
  nixpkgs,
  system,
  compiler,
  packageName,
  ...
}:
let
  haskellOverlays = [
    (
      final: prev:
      let
        prevHaskellPackages = prev.haskell.packages.${compiler};
      in
      prev.lib.attrsets.recursiveUpdate prev {
        haskell.packages.${compiler} = prevHaskellPackages.override {
          ghc = prevHaskellPackages.ghc.override {
            enableRelocatedStaticLibs = true;
            enableShared = false;
            enableDwarf = false;
            enableProfiledLibs = false;
            enableDocs = false;
            enableNativeBignum = true;
          };
          buildHaskellPackages = prevHaskellPackages.buildHaskellPackages.override (old: {
            ghc = final.haskell.packages.${compiler}.ghc;
            buildHaskellPackages = final.haskell.packages.${compiler};
          });
        };
      }
    )
    (final: prev: {
      haskellPackages = prev.haskell.packages.${compiler};
      ghc = prev.haskell.packages.${compiler}.ghc;
    })
    (final: prev: {
      haskell = prev.haskell // {
        packageOverrides = prev.lib.composeExtensions prev.haskell.packageOverrides (
          hfinal: hprev: {
            mkDerivation =
              args:
              hprev.mkDerivation (
                args
                // {
                  doCheck = false;
                  doHaddock = false;
                  enableLibraryProfiling = false;
                  enableExecutableProfiling = false;
                }
              );
          }
        );
      };
    })
  ];
  pkgs = import nixpkgs {
    inherit system;
    overlays = haskellOverlays;
  };
  inherit (pkgs) pkgsMusl;

  libffi = pkgsMusl.libffi.overrideAttrs { dontDisableStatic = true; };
  libpq = (pkgsMusl.libpq.overrideAttrs { dontDisableStatic = true; }).dev;
  numactl = pkgsMusl.numactl.overrideAttrs (oldAttrs: {
    configureFlags = (oldAttrs.configureFlags or [ ]) ++ [
      "--enable-static"
      "--disable-shared"
    ];
  });
  zlib = pkgsMusl.zlib.static;

  haskellPackages = pkgsMusl.haskell.packages.${compiler};
  haskellLib = pkgsMusl.haskell.lib.compose;

  package = haskellPackages.callCabal2nix packageName (pkgsMusl.lib.cleanSource ./.) { };
in
{
  nativeBuildInputs = [
    haskellPackages.ghc
    libffi
    libpq
    numactl
    pkgs.buildPackages.lld
    pkgs.upx
    zlib
  ];
  package = pkgsMusl.lib.pipe package [
    haskellLib.dontHaddock
    haskellLib.dontHyperlinkSource
    haskellLib.dontCoverage
    haskellLib.disableExecutableProfiling
    haskellLib.disableLibraryProfiling
    haskellLib.disableSharedLibraries
    haskellLib.justStaticExecutables
    haskellLib.enableDeadCodeElimination
    (haskellLib.overrideCabal (old: {
      enableParallelBuilding = true;
      buildTools = (old.buildTools or [ ]) ++ [ pkgs.buildPackages.lld ];
    }))
    (haskellLib.appendConfigureFlags [
      "-O2"
      "--ghc-option=-fPIC"
      "--ghc-option=-optl=-static"
      "--ghc-option=-split-sections"
      "--ghc-option=-optl-fuse-ld=lld"
      "--ld-option=-fuse-ld=lld"
      "--with-ld=ld.lld"
      "--ld-option=-Wl,--gc-sections,--build-id,--icf=all"
      "--extra-lib-dirs=${libffi}/lib"
      "--extra-lib-dirs=${zlib}/lib"
      "--extra-lib-dirs=${numactl}/lib"
      "--extra-lib-dirs=${libpq}/lib"
    ])
    (
      src:
      pkgsMusl.stdenv.mkDerivation {
        name = "${src.name}-compressed";
        inherit src;
        nativeBuildInputs = [ pkgs.upx ];
        installPhase = ''
          mkdir -p $out
          cp -R $src/. $out
          chmod -R +w $out/bin
          upx -q --lzma -1 $out/bin/*
          chmod -R -w $out/bin
        '';
      }
    )
  ];
}
