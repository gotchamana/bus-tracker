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
    in
    {
      devShells = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
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
              pkgs.watchexec
              pkgs.pkg-config
            ];
            buildInputs = with pkgs; [
              libpq
              liquibase
              postgresql_18
              zlib
            ];
          };
        }
      );
      packages = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hpkgs = pkgs.haskell.packages.ghc9124;
          hpkgs' = hpkgs.extend (
            self: super: {
              haskell-language-server = self.callHackageDirect {
                pkg = "haskell-language-server";
                ver = "2.14.0.0";
                sha256 = "sha256-e7pa/QGSqyaxVowGE6DIDrMT/OYTsJL96w40rVgIz3Q=";
              } { };
              ghcide = self.callHackageDirect {
                pkg = "ghcide";
                ver = "2.14.0.0";
                sha256 = "sha256-QBsLOV9YaaJFZO2NUQzmEv3FJ6KGJnGeRVWvMdvEyyA=";
              } { };
            }
          );
        in
        {
          default = hpkgs'.haskell-language-server;
        }
      );
    };
}
