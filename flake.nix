{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs =
    { haskellNix, nixpkgs, ... }:
    let
      eachSystem =
        systems: f:
        builtins.foldl' (
          attrs: system:
          let
            ret = f system;
          in
          builtins.foldl' (
            attrs: key:
            attrs
            // {
              ${key} = (attrs.${key} or { }) // {
                ${system} = ret.${key};
              };
            }
          ) attrs (builtins.attrNames ret)
        ) { } systems;
      supportedSystems = [
        "aarch64-linux"
        "x86_64-linux"
      ];
    in
    eachSystem supportedSystems (
      system:
      let
        packageName = "bus-tracker";
        overlays = [
          haskellNix.overlay
          (final: prev: {
            haskell-nix = prev.haskell-nix // {
              compiler = prev.haskell-nix.compiler // {
                ghc9103 = prev.haskell-nix.compiler.ghc9103.override {
                  enableNativeBignum = true;
                };
              };
            };
          })
          (final: prev: {
            pkgsCross = prev.pkgsCross // {
              musl64 = prev.pkgsCross.musl64.extend (
                self: super: {
                  postgresql = super.postgresql.override {
                    curlSupport = false;
                    gssSupport = false;
                    icuSupport = false;
                    jitSupport = false;
                    pamSupport = false;
                    perlSupport = false;
                    pythonSupport = false;
                    tclSupport = false;
                  };
                }
              );
            };
          })
          (final: prev: {
            ${packageName} = import ./pkgs/package.nix {
              pkgs = final;
              src = ./.;
              compiler = "ghc9103";
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.${packageName}.flake {
          crossPlatforms =
            ps: with ps; [
              musl64
            ];
        };
      in
      pkgs.lib.recursiveUpdate flake {
        legacyPackages = pkgs;
        devShells.default = pkgs.${packageName}.shellFor { };
        packages.default = flake.packages."${packageName}:exe:${packageName}";
      }
    );

  nixConfig = {
    cores = 3;
    max-jobs = 3;
    extra-substituters = [
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}
