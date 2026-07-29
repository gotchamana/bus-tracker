{
  pkgs,
  src,
  compiler,
  ...
}:
let
  staticModule = { pkgs, lib, ... }: {
    packages.bus-tracker.components.exes.bus-tracker =
      lib.optionalAttrs pkgs.stdenv.hostPlatform.isMusl
        {
          ghcOptions = [
            "-optl=-fuse-ld=lld"
            "-optl=-lpgcommon"
            "-optl=-lpgport"
            "-optl=-lssl"
            "-optl=-lcrypto"
            "-optl=-L${pkgs.openssl.out}/lib"
          ];
          build-tools = [
            pkgs.buildPackages.lld
          ];
        };
  };
in
pkgs.haskell-nix.project' {
  inherit src;
  compiler-nix-name = compiler;
  cabalProjectLocal = ''
    package *
      documentation: true
  '';
  cabalProjectFreeze = builtins.readFile ../cabal.project.freeze;
  modules = [ staticModule ];
  shell = {
    withHoogle = false;
    tools = {
      cabal-install = { };
      cabal-plan = {
        cabalProjectLocal = ''
          package cabal-plan
            flags: +exe
        '';
      };
      fourmolu = { };
      graphmod = { };
      haskell-language-server = { };
      hlint = { };
    };
    nativeBuildInputs = with pkgs; [
      graphviz
      haskellPackages.cabal-fmt
      watchexec
    ];
    buildInputs = with pkgs; [
      liquibase
      postgresql_18
    ];
    crossPlatforms =
      ps: with ps; [
        musl64
      ];
  };
}
