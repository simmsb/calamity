{
  description = "calamity";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # hls.url = "github:haskell/haskell-language-server";
    # hls.url = "github:cydparser/haskell-language-server?rev=497d2846ec4aceea8e368209d9ed2b13a405abf0";
    
    # flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-flake.url = "github:srid/treefmt-flake";
  };

  outputs = inputs @ { self, nixpkgs, gitignore, flake-parts, haskell-flake, treefmt-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-flake.flakeModule
      ];
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          haskellPackages = pkgs.haskell.packages.ghc925;
          packages = {
            calamity-commands.root = ./calamity-commands;
            calamity.root = ./calamity;
          };

          buildTools = hp:
            let
              # https://github.com/NixOS/nixpkgs/issues/140774 reoccurs in GHC 9.2
              workaround140774 = hpkg: with pkgs.haskell.lib;
                overrideCabal hpkg (drv: {
                  enableSeparateBinOutput = false;
                });
            in
            {
              inherit (pkgs)
                treefmt
                nixpkgs-fmt;
              inherit (hp)
                cabal-fmt;
              inherit (hp)
                fourmolu;
              ghcid = workaround140774 hp.ghcid;
            } // config.treefmt.formatters;

          overrides = self: super: with pkgs.haskell.lib; {
            ListLike = dontCheck super.ListLike;
            type-errors = dontCheck (super.callHackage "type-errors" "0.2.0.0" { });
            polysemy-plugin = dontCheck (super.callHackage "polysemy-plugin" "0.4.3.1" { });
            polysemy = dontCheck (super.callHackage "polysemy" "1.7.1.0" { });
            PyF = dontCheck (super.callHackage "PyF" "0.11.0.0" { });
            typerep-map = dontCheck (super.callHackage "typerep-map" "0.6.0.0" { });
            aeson-optics = dontCheck (super.callHackage "aeson-optics" "1.2.0.1" { });
          };
          hlsCheck.enable = true;
          hlintCheck.enable = true;
        };
        treefmt.formatters = {
          inherit (pkgs)
            nixpkgs-fmt;
          inherit (pkgs.haskellPackages)
            cabal-fmt
            fourmolu;
        };
        packages.default = self'.packages.calamity;
      };
    };
}
