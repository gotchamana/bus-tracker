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
              fourmolu
              haskell-language-server
              hlint
            ];
            buildInputs = with pkgs; [ zlib ];
          };
        }
      );
    };
}
