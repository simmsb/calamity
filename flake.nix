{
  description = "calamity";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    gitignore.url = "github:hercules-ci/gitignore.nix";
    gitignore.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";

    haskell-flake.url = "github:srid/haskell-flake";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    flake-root.url = "github:srid/flake-root";

    nixpkgs-140774-workaround.url = "github:srid/nixpkgs-140774-workaround";
  };

  outputs = inputs@{ self, nixpkgs, gitignore, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      # systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
      ];
      perSystem = { self', lib, config, pkgs, ... }: {
        haskellProjects.default = {
          imports = [
            inputs.nixpkgs-140774-workaround.haskellFlakeProjectModules.default
          ];

          basePackages = pkgs.haskell.packages.ghc944.override (old: { overrides = (final: prev: { ormolu = final.ormolu_0_5_3_0; }); });

          packages = {
            calamity-commands.root = ./calamity-commands;
            calamity.root = ./calamity;
          };

          devShell = {
            tools = hp:
              {
                ghcid = hp.ghcid;
                treefmt = config.treefmt.build.wrapper;
              } // config.treefmt.build.programs;
            hlsCheck.enable = false;
          };

          overrides = self: super: with pkgs.haskell.lib; {
            ghcid = dontCheck super.ghcid;

            ListLike = dontCheck super.ListLike;
            type-errors = dontCheck (super.callHackage "type-errors" "0.2.0.1" { });
            polysemy-plugin = dontCheck (super.callHackage "polysemy-plugin" "0.4.4.0" { });
            polysemy = dontCheck (super.callHackage "polysemy" "1.9.0.0" { });
            typerep-map = dontCheck (super.callHackage "typerep-map" "0.6.0.0" { });
            aeson-optics = dontCheck (super.callHackage "aeson-optics" "1.2.0.1" { });
            pretty-simple = dontCheck (super.callHackage "pretty-simple" "4.1.2.0" { });
            di-core = dontCheck super.di-core;
            optics = dontCheck super.optics;

            http-api-data = dontCheck (super.callHackage "http-api-data" "0.5" { });
            # this v is needed so http-api-data builds
            attoparsec-iso8601 = dontCheck (super.callHackage "attoparsec-iso8601" "1.1.0.0" { });
          };
        };

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        packages.default = self'.packages.calamity;
      };

      flake.haskellFlakeProjectModules = {
        output = { pkgs, ... }: {
          source-overrides = {
            calamity = self + /calamity;
            calamity-commands = self + /calamity-commands;
          };
        };
      };
    };
}
