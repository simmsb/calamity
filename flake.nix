{
  description = "calamity";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    gitignore.url = "github:hercules-ci/gitignore.nix";
    gitignore.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";

    haskell-flake.url = "github:srid/haskell-flake";
    check-flake.url = "github:srid/check-flake";

    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    flake-root.url = "github:srid/flake-root";

    # req.url = "github:mrkkrp/req";
    # req.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, gitignore, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      # systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.check-flake.flakeModule
      ];
      perSystem = { self', lib, config, pkgs, ... }: {
        haskellProjects.default = {
          #basePackages = pkgs.haskell.packages.ghc981;

          packages = {
            type-errors.source = "0.2.0.2";
            websockets.source = "0.13.0.0";
            # crypton-connection.source = "0.3.1";
            # crypton-x509-system.source = "1.6.7";
            # crypton-x509.source = "1.7.6";
            # tls.source = "1.7.0";
            # req.source = inputs.req;
            # http-client-tls.source = "0.3.6.2";
          };

          settings = {
            aeson-optics.jailbreak = true;
            type-errors.check = false;
            ListLike.check = false;
            di-core.check = false;
            optics.check = false;
            crypton-x509.check = false;
            vector.check = false;
            ghcid.check = false;

            haskell-language-server.custom = with pkgs.haskell.lib.compose; lib.flip lib.pipe [
              (disableCabalFlag "floskell")
              (disableCabalFlag "ormolu")
              (drv: drv.override { hls-ormolu-plugin = null; })
              (drv: drv.override { hls-floskell-plugin = null; })
            ];
          };

          devShell = {
            tools = hp: { ghcid = null; };

            hoogle = false;
            hlsCheck.enable = false;
          };

          autoWire = [ "packages" "apps" ];
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
        devShells.default = pkgs.mkShell {
          name = "calamity-devshell";

          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.treefmt.build.devShell
          ];
        };
      };
    };
}
