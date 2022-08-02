{
  description = "calamity";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.hls.url = "github:haskell/haskell-language-server";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;
  inputs.flake-compat.inputs.nixpkgs.follows = "nixpkgs";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, hls, gitignore }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        commands-sources = gitignore.lib.gitignoreSource ./calamity-commands/.;
        main-sources = gitignore.lib.gitignoreSource ./calamity-commands/.;
      in
      with pkgs; rec {

        calamityBuilder = hPkgs:
          let
            shell = pkg.env.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ cabal-install zlib ];
            });

            # Shell with haskell language server
            shell_hls = shell.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ hPkgs.haskell-language-server ];
            });

            calamity-commands = hPkgs.callCabal2nix "calamity-commands" commands-sources { };

            hPkgs' = hPkgs.override (old: {
              overrides = lib.composeExtensions (old.overrides or (_: _: { })) (self: super: {
                # inherit calamity-commands;
              });
            });

            pkg = (haskell.lib.buildFromSdist
              (hPkgs'.callCabal2nix "calamity" main-sources { })).overrideAttrs
              (oldAttrs: {
                buildInputs = oldAttrs.buildInputs;
                passthru = oldAttrs.passthru // { inherit shell shell_hls; };
              });
            # Add the GHC version in the package name
          in
          pkg.overrideAttrs (old: { name = "calamity-ghc${hPkgs.ghc.version}"; });

        packages = rec {
          calamity_810 = calamityBuilder (haskell.packages.ghc8107.override {
            overrides = self: super: with haskell.lib; { };
          });

          calamity_92 = calamityBuilder (haskell.packages.ghc923.override {
            overrides = self: super: with haskell.lib; {
              type-errors = haskell.lib.dontCheck (super.callHackage "type-errors" "0.2.0.0" { });
              polysemy-plugin = haskell.lib.dontCheck (super.callHackage "polysemy-plugin" "0.4.3.1" { });
              polysemy = haskell.lib.dontCheck (super.callHackage "polysemy" "1.7.1.0" { });
            };
          });

          calamity_all = linkFarmFromDrvs "calamity_all" [
            calamity_810
            calamity_92
          ];

          calamity_current = calamity_92;
          calamity = calamity_current;
        };

        defaultPackage = packages.calamity;
        devShell = packages.calamity.shell_hls;
        devShells = builtins.mapAttrs (name: value: value.shell) packages;
      });
}
