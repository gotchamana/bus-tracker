{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-26.05";
  };

  outputs =
    { nixpkgs, ... }:
    let
      supportedSystems = [
        "aarch64-linux"
        "x86_64-linux"
      ];
      eachSystem = f: nixpkgs.lib.genAttrs supportedSystems f;
      overlay = final: prev: {
        all-cabal-hashes = prev.fetchurl (finalAttrs: {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/4356dd54ddac0d8dc77ab578bfeac5f9148f7b16.tar.gz";
          sha256 = "YVRxNZkKIOODRoAo39NIqEs/1vY4CPjW7cYAQIgAeVk=";
          name = "${finalAttrs.pname}-${finalAttrs.version}.tar.gz";
          pname = "all-cabal-hashes";
          version = "4356dd5";
          passthru.updateScript = prev.all-cabal-hashes.updateScript;
          meta = prev.all-cabal-hashes.meta;
        });
      };
    in
    {
      devShells = eachSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
          hpkgs = pkgs.haskell.packages.ghc9103;
        in
        {
          default = hpkgs.shellFor {
            packages = _: [ ];
            nativeBuildInputs = with hpkgs; [
              cabal-fmt
              cabal-install
              cabal-plan
              fourmolu
              haskell-language-server
              hlint
            ];
            buildInputs = with pkgs; [ zlib ];
          };
        }
      );
      packages = eachSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
          hpkgs = pkgs.haskell.packages.ghc9124;
          hpkgs' = hpkgs.extend (
            self: super: {
              haskell-language-server = self.callHackage "haskell-language-server" "2.14.0.0" { };
            }
          );
        in
        {
          default = hpkgs'.haskell-language-server;
        }
      );
    };
}
