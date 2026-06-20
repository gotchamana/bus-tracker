{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-26.05";
  };

  outputs =
    { nixpkgs, ... }:
    let
      system = "aarch64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      hpkgs = pkgs.haskell.packages.ghc9103;
    in
    {
      devShells.${system}.default = hpkgs.shellFor {
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
    };
}
