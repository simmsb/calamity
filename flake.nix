{
  description = "calamity";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  # inputs.hls.url = "github:haskell/haskell-language-server";
  inputs.hls.url = "github:cydparser/haskell-language-server?rev=497d2846ec4aceea8e368209d9ed2b13a405abf0";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, hls, gitignore }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        commands-sources = gitignore.lib.gitignoreSource ./calamity-commands/.;
        main-sources = gitignore.lib.gitignoreSource ./calamity/.;
        calamityCommandsPkg = hPkgs: hPkgs.callCabal2nix "calamity-commands" commands-sources { };
        calamityPkg = hPkgs:
          let
            hPkgs' = hPkgs.override (old: {
              overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) (self: super: {
                calamity-commands = calamityCommandsPkg hPkgs;
              });
            });
          in
          hPkgs'.callCabal2nix "calamity" main-sources { };
      in
      with pkgs;
      let
        calamityBuilder = { hls ? null, hPkgs }:
          let
            shell = pkg.env.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs
                ++ [ cabal-install zlib hPkgs.hoogle ];
            });

            # Shell with haskell language server
            shell_hls =
              if hls == null then
                { } else
                {
                  shell_hls = shell.overrideAttrs (old: {
                    nativeBuildInputs = old.nativeBuildInputs
                      ++ [ hls ];
                  });
                };

            pkg = (haskell.lib.buildFromSdist
              (calamityPkg hPkgs)).overrideAttrs
              (oldAttrs: {
                buildInputs = oldAttrs.buildInputs;
                passthru = oldAttrs.passthru // { inherit shell; } // shell_hls;
              });
            # Add the GHC version in the package name
          in
          pkg.overrideAttrs (old: { name = "calamity-ghc${hPkgs.ghc.version}"; });
        sharedPackages = super: {
          aeson-optics = haskell.lib.dontCheck (super.callHackage "aeson-optics" "1.2" { });
        };
      in
      rec {
        lib = {
          inherit calamityCommandsPkg calamityPkg;
        };

        packages =
          rec {
            calamity_810 = calamityBuilder {
              hPkgs =
                (haskell.packages.ghc8107.override {
                  overrides = self: super: with haskell.lib; (sharedPackages super) // { };
                });
            };

            calamity_92 = calamityBuilder {
              hls = hls.allPackages.${system}.haskell-language-server-925;
              hPkgs = (haskell.packages.ghc925.override {
                overrides = self: super: with haskell.lib; (sharedPackages super) // {
                  ListLike = haskell.lib.dontCheck super.ListLike;
                  type-errors = haskell.lib.dontCheck (super.callHackage "type-errors" "0.2.0.0" { });
                  polysemy-plugin = haskell.lib.dontCheck (super.callHackage "polysemy-plugin" "0.4.3.1" { });
                  polysemy = haskell.lib.dontCheck (super.callHackage "polysemy" "1.7.1.0" { });
                  PyF = haskell.lib.dontCheck (super.callHackage "PyF" "0.11.0.0" { });
                };
              });
            };

            calamity_all = linkFarmFromDrvs "calamity_all" [
              calamity_810
              calamity_92
            ];

            calamity_current = calamity_92;
            calamity = calamity_current;
          };

        packages.default = packages.calamity;
        devShells.default = packages.calamity.shell_hls;
      });
}
