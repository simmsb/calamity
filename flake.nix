{
  description = "calamity";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
  };

  outputs = inputs@{ self, nixpkgs, gitignore, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      # systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = { self', lib, config, pkgs, ... }: {
        haskellProjects.main = {
          haskellPackages = pkgs.haskell.packages.ghc944;
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
            ghcid = workaround140774 hp.ghcid;
            treefmt = config.treefmt.build.wrapper;
          } // config.treefmt.build.programs;
          hlsCheck.enable = false;
          hlintCheck.enable = true;

          overrides = self: super: with pkgs.haskell.lib; {
            ghcid = dontCheck super.ghcid;

            ListLike = dontCheck super.ListLike;
            type-errors = dontCheck (super.callHackage "type-errors" "0.2.0.1" { });
            polysemy-plugin = dontCheck (super.callHackage "polysemy-plugin" "0.4.4.0" { });
            polysemy = dontCheck (super.callHackage "polysemy" "1.9.0.0" { });
            typerep-map = dontCheck (super.callHackage "typerep-map" "0.6.0.0" { });
            aeson-optics = dontCheck (super.callHackage "aeson-optics" "1.2.0.1" { });
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

        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8690
              hoogle serve -p 8690 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = "${lib.getExe config.treefmt.build.wrapper}";
            category = "Dev Tools ";
          };
          run = {
            description = "Run the project with ghcid auto-recompile";
            exec = ''
              ghcid -c "cabal repl exe:haskell-template" --warnings -T :main
            '';
            category = "Primary";
          };
        };

        packages.default = self'.packages.main-calamity;
        devShells.default =
          config.mission-control.installToDevShell self'.devShells.main;
      };

      flake.herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
