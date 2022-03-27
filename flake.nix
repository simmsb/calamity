{
  description = "calamity";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;
  inputs.flake-compat.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils, flake-compat, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            calamity =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                shell.tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                };
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
                  haskellPackages.implicit-hie
                ];
              };
            # calamity-commands =
            #   final.haskell-nix.cabalProject' {
            #     src = ./calamity-commands;
            #     compiler-nix-name = "ghc8107";
            #     shell.tools = {
            #       cabal = {};
            #       hlint = {};
            #       haskell-language-server = {};
            #     };
            #     shell.buildInputs = with pkgs; [
            #       nixpkgs-fmt
            #     ];
            #   };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.calamity.flake { };
      in
      flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."calamity:lib:calamity";
      });
}
