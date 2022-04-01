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
                compiler-nix-name = "ghc922";
                shell.tools = {
                  cabal = { };
                  hlint = { };
                  haskell-language-server = { };
                };
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
                  haskellPackages.implicit-hie
                ];
                modules = [{
                  nonReinstallablePkgs = [
                    "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
                    "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
                    # ghcjs custom packages
                    "ghcjs-prim" "ghcjs-th"
                    "ghc-bignum" "exceptions" "stm"
                    "ghc-boot"
                    "ghc" "Cabal" "Win32" "array" "binary" "bytestring" "containers"
                    "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
                    # "ghci" "haskeline"
                    "hpc"
                    "mtl" "parsec" "process" "text" "time" "transformers"
                    "unix" "xhtml" "terminfo"
                  ];
                }];
              };
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
