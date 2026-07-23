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
      compiler = "ghc9103";
      packageName = "bus-tracker";
    in
    {
      devShells = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          haskellPackages = pkgs.haskell.packages.${compiler}.override {
            overrides = final: prev: {
              ${packageName} = prev.callCabal2nix packageName (pkgs.lib.cleanSource ./.) { };
              adjunctions = prev.callHackageDirect {
                pkg = "adjunctions";
                ver = "4.4.4";
                sha256 = "sha256-XuSwBa7Qum/uXKp4Aq6GSbU3ltrQsQUKl0rPpSovFy8=";
              } { };
              aeson = prev.callHackageDirect {
                pkg = "aeson";
                ver = "2.2.5.0";
                sha256 = "sha256-RooTSc8dZrcUiWFQDssKv1/l5OB/psUD7jbyWG/7VKQ=";
              } { };
              aeson-better-errors = prev.callHackageDirect {
                pkg = "aeson-better-errors";
                ver = "0.9.1.4";
                sha256 = "sha256-IAJ0Yf9Zv4VcKg3jjuTHLiQfAQXooAhj+7vGD9K8T24=";
              } { };
              async = prev.callHackageDirect {
                pkg = "async";
                ver = "2.2.6";
                sha256 = "sha256-1jcK5ud/LBBGCFyueroqhhbU4SXjirPCM3jq6CK7YZw=";
              } { };
              beam-core = prev.callHackageDirect {
                pkg = "beam-core";
                ver = "0.11.1.0";
                sha256 = "sha256-YPQmPOA5Ryl1ukl/H99nS4wB7SXmJcBaJPn9ARVS67M=";
              } { };
              beam-migrate = prev.callHackageDirect {
                pkg = "beam-migrate";
                ver = "0.6.0.0";
                sha256 = "sha256-hr0m4karSiEaSCYxad3ZRKNfUCfKdAfnQbnrH16BSV0=";
              } { };
              beam-postgres = prev.callHackageDirect {
                pkg = "beam-postgres";
                ver = "0.6.3.0";
                sha256 = "sha256-M+bFSTEwkQEzxScFomQIi50OQrpcGo74SqEZbUglT+c=";
              } { };
              concise = prev.callHackageDirect {
                pkg = "concise";
                ver = "0.1.0.1";
                sha256 = "sha256-GAPmkTzhS3orfPVDHj4op8x5Czw9Pb2dOQXzXMDxD2k=";
              } { };
              conduit = prev.callHackageDirect {
                pkg = "conduit";
                ver = "1.3.6.1";
                sha256 = "sha256-3VHVCwBj+MLLMnX1Mezzn0X2SVrtKjPR8m8R1fWaHRo=";
              } { };
              file-io = prev.callHackageDirect {
                pkg = "file-io";
                ver = "0.2.0";
                sha256 = "sha256-AtH+vo/UDulZzcS5ZQpqY9tRHb6gP/l96OyZHoO2K78=";
              } { };
              free = prev.callHackageDirect {
                pkg = "free";
                ver = "5.2";
                sha256 = "sha256-AcvcPGgeZGnAd6C6DxAjTLEcJVY1fERK5NpzCeA0xCw=";
              } { };
              http-api-data = prev.callHackageDirect {
                pkg = "http-api-data";
                ver = "0.7";
                sha256 = "sha256-PMPoIR72fJoMzL9LcuPEsDE8UR6MJIsLvPDmIEwzQ7Q=";
              } { };
              http-semantics = prev.callHackageDirect {
                pkg = "http-semantics";
                ver = "0.4.1";
                sha256 = "sha256-jzNgENa0Uj0ZGfg0N6zfbP2crSfRjBNpikKGgEl1hl4=";
              } { };
              http-types = prev.callHackageDirect {
                pkg = "http-types";
                ver = "0.12.5";
                sha256 = "sha256-Y1/wrRFPIVxgTGWgPboRDUht+fzvl3e1jazj+G1pTw0=";
              } { };
              http2 = prev.callHackageDirect {
                pkg = "http2";
                ver = "5.4.0";
                sha256 = "sha256-PeEWVd61bQ8G7LvfLeXklzXqNJFaAjE2ecRMWJZESPE=";
              } { };
              indexed-traversable-instances = prev.callHackageDirect {
                pkg = "indexed-traversable-instances";
                ver = "0.1.2.1";
                sha256 = "sha256-A+QsMej4D01DA5cqCRY7yML8auUD9zNNhdr263ySWsc=";
              } { };
              invariant = prev.callHackageDirect {
                pkg = "invariant";
                ver = "0.6.5";
                sha256 = "sha256-fWIyR0QR73Qy10RbQ3OpOI7ZCMWvBkCNsFOO2OKHMas=";
              } { };
              jose = prev.callHackageDirect {
                pkg = "jose";
                ver = "0.12";
                sha256 = "sha256-hAQUFB6QRH9nadDXUo4d8jBi7en0BU0dufCYPFO0KQA=";
              } { };
              kan-extensions = prev.callHackageDirect {
                pkg = "kan-extensions";
                ver = "5.2.8";
                sha256 = "sha256-bAXxyQxLfZN6gMdJCKx55oa5QXiy5ixmZC9oasooUgA=";
              } { };
              lens = prev.callHackageDirect {
                pkg = "lens";
                ver = "5.3.6";
                sha256 = "sha256-ynsaiAIMADeRl0rN7+F9FY1qrpWWpgVzpwN5z3A9550=";
              } { };
              mono-traversable = prev.callHackageDirect {
                pkg = "mono-traversable";
                ver = "1.0.21.0";
                sha256 = "sha256-qI9oKwDxJCNiRw7bNfutDy0qQxf+T7v4OSyGhd4UJOo=";
              } { };
              optparse-applicative = prev.callHackageDirect {
                pkg = "optparse-applicative";
                ver = "0.19.0.0";
                sha256 = "sha256-dhqvRILfdbpYPMxC+WpAyO0KUfq2nLopGk1NdSN2SDM=";
              } { };
              postgresql-simple = prev.callHackageDirect {
                pkg = "postgresql-simple";
                ver = "0.7.0.1";
                sha256 = "sha256-n3+4ejU03RXLVonbgKknSL9SgW/ZRVwMehYuXH30p7E=";
              } { };
              rerefined = prev.callHackageDirect {
                pkg = "rerefined";
                ver = "0.8.0";
                sha256 = "sha256-l98BO4IN3z0fn4CP8yzD0QYXOjTkIQLBj+7rt23HE3U=";
              } { };
              resource-pool = prev.callHackageDirect {
                pkg = "resource-pool";
                ver = "0.5.0.1";
                sha256 = "sha256-51KDyA6EtBqD+BmAXYZ0S4h5J2Qkvh5ZYQFcuhe9BHk=";
              } { };
              semialign = prev.callHackageDirect {
                pkg = "semialign";
                ver = "1.4";
                sha256 = "sha256-IPJ53dMdiLcnAGG2+MN8m+hkYydzfdPXXzrNOl+RVsE=";
              } { };
              semigroupoids = prev.callHackageDirect {
                pkg = "semigroupoids";
                ver = "6.0.2";
                sha256 = "sha256-dJjTDs6+zzOV5VCr1z4oJylmzIJq3fEtubbbOE5hglk=";
              } { };
              servant = prev.callHackageDirect {
                pkg = "servant";
                ver = "0.20.3.0";
                sha256 = "sha256-cVNjD1tEQnj1pWoFKKtH1vVEZMZUHHsYD9F9hza/920=";
              } { };
              servant-server = prev.callHackageDirect {
                pkg = "servant-server";
                ver = "0.20.3.0";
                sha256 = "sha256-HYWXEesvzAxsSst3VK85H7o1SpfjrNLQDiPOrossbRQ=";
              } { };
              streaming-commons = prev.callHackageDirect {
                pkg = "streaming-commons";
                ver = "0.2.3.1";
                sha256 = "sha256-Gl2eaJcWe1sxmcE/octWlH9uSnERguf+5H66K4fV87s=";
              } { };
              text-show = prev.callHackageDirect {
                pkg = "text-show";
                ver = "3.11.4";
                sha256 = "sha256-PTinCmnWZXfaSQREDdqHLxvrhCM24qqZiTVoZ6ifnrA=";
              } { };
              unordered-containers = prev.callHackageDirect {
                pkg = "unordered-containers";
                ver = "0.2.21";
                sha256 = "sha256-YGklj5D14AvkiaVkJH44E7YjSb8wivPrHZz3vgsmSFk=";
              } { };
              vault = prev.callHackageDirect {
                pkg = "vault";
                ver = "0.3.2.0";
                sha256 = "sha256-19JR3pTodkwDQ6MVg4CJOQAOWyJicN2YMQihDoxGmiY=";
              } { };
              vector-sized = prev.callHackageDirect {
                pkg = "vector-sized";
                ver = "1.6.1";
                sha256 = "sha256-//EOAwpEEQkdYF88U/bp0uybKleYHRmTWaKsxIZvCeQ=";
              } { };
              wai = prev.callHackageDirect {
                pkg = "wai";
                ver = "3.2.4";
                sha256 = "sha256-NARmVhT5G1eMdtMM1xp7RFpevunThAB4tltCMih+qu8=";
              } { };
              wai-app-static = prev.callHackageDirect {
                pkg = "wai-app-static";
                ver = "3.2.1";
                sha256 = "sha256-HQP76brA2MwBVipRs2ZstY59nUMhzfJkw90Kaep2o9I=";
              } { };
              wai-extra = prev.callHackageDirect {
                pkg = "wai-extra";
                ver = "3.1.18";
                sha256 = "sha256-aar/BoX34KSvS06owvQkpfjsoTx63KY9NWbfBQclRS0=";
              } { };
              wai-logger = prev.callHackageDirect {
                pkg = "wai-logger";
                ver = "2.5.0";
                sha256 = "sha256-d7iq3rxKSLECOdTvQcNQibwXPi87C69wCzpPvv7VD9A=";
              } { };
              warp = prev.callHackageDirect {
                pkg = "warp";
                ver = "3.4.14";
                sha256 = "sha256-RnoOUlC6dOP0sK/tYAJCX1oLzVFG1GILUY+yVbmvW8Y=";
              } { };
              witherable = prev.callHackageDirect {
                pkg = "witherable";
                ver = "0.5";
                sha256 = "sha256-vX0ePzUuYcL+gpgaEeqhmtiZO886QLs7gB9U53E7p3Y=";
              } { };
            };
          };
        in
        {
          default = haskellPackages.shellFor {
            packages = hpkgs: [ hpkgs.${packageName} ];
            nativeBuildInputs = with haskellPackages; [
              cabal-fmt
              cabal-install
              cabal-plan
              fourmolu
              graphmod
              haskell-language-server
              hlint
              pkgs.graphviz
              pkgs.pkg-config
              pkgs.watchexec
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
          static = import ./package-static.nix {
            inherit
              nixpkgs
              system
              compiler
              packageName
              ;
          };
        in
        {
          default = static.package;
        }
      );
    };

  nixConfig = {
    cores = 2;
    max-jobs = 1;
  };
}
