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
        modules = [
          ({ lib, ... }: {
            # https://github.com/input-output-hk/haskell.nix/issues/829
            config.dontStrip = false;
            # config.reinstallableLibGhc = true;
            options.nonReinstallablePkgs = lib.mkOption {
              apply = x: [ "exceptions" "stm" ] ++ x;
            };
          })
        ];
        overlays = [
          haskellNix.overlay
          (final: prev: {
            calamity =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc922";
                shell.tools = {
                  cabal = {
                    version = "latest";
                    inherit modules;
                  };
                  hlint = {
                    version = "latest";
                    inherit modules;
                  };
                  haskell-language-server = {
                    version = "latest";
                    inherit modules;
                    cabalProject = ''
                      packages:
                               ./

                      with-compiler: ghc-9.2.2

                      tests: true

                      package *
                        ghc-options: -haddock
                        test-show-details: direct

                      write-ghc-environment-files: never

                      constraints:
                        -- These plugins don't build/work on GHC92 yet
                        haskell-language-server
                          +ignore-plugins-ghc-bounds
                          -brittany
                          -haddockComments
                          -hlint
                          -retrie
                          -splice
                          -tactic,

                      allow-newer:
                        -- for shake-bench
                        Chart:lens,
                        Chart-diagrams:lens,

                        -- for head.hackage
                        primitive-unlifted:base,

                        brittany:ghc-boot,
                        brittany:ghc-boot-th,
                        brittany:ghc,
                        brittany:ghc-exactprint,
                        brittany:bytestring,
                        brittany:base,
                        -- https://github.com/lspitzner/multistate/pull/8
                        multistate:base,
                        -- https://github.com/lspitzner/data-tree-print/pull/3
                        data-tree-print:base,
                        -- https://github.com/lspitzner/butcher/pull/8
                        butcher:base,

                        ormolu:ghc-lib-parser,

                        fourmolu:ghc-lib-parser,
                        fourmolu:Cabal,

                        hls-hlint-plugin:ghc-lib,
                        hls-hlint-plugin:ghc-lib-parser,
                        hls-hlint-plugin:ghc-lib-parser-ex,
                        hlint:ghc-lib-parser,
                        hlint:ghc-lib-parser-ex,
                        -- See https://github.com/mpickering/apply-refact/pull/116
                        apply-refact:base,

                        implicit-hie-cradle:bytestring,
                        implicit-hie-cradle:time,

                        -- For tactics
                        ghc-source-gen:ghc,

                        -- for ghcide:test via ghc-typelits-knownnat
                        ghc-typelits-natnormalise:ghc-bignum,

                        hiedb:base

                      allow-older:
                        primitive-extras:primitive-unlifted
                    '';
                  };
                };
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
                  haskellPackages.implicit-hie
                ];
                modules = modules;
                # modules = [{
                #   reinstallableLibGhc = true;
                # #   nonReinstallablePkgs = [
                # #     "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
                # #     "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
                # #     # ghcjs custom packages
                # #     "ghcjs-prim" "ghcjs-th"
                # #     "ghc-bignum" "exceptions" "stm"
                # #     "ghc-boot"
                # #     "ghc" "Cabal" "Win32" "array" "binary" "bytestring" "containers"
                # #     "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
                # #     # "ghci" "haskeline"
                # #     "hpc"
                # #     "mtl" "parsec" "process" "text" "time" "transformers"
                # #     "unix" "xhtml" "terminfo"
                # #   ];
                # }];
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
