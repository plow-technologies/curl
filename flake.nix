{
  description = "Flake for curl-jr";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt.url = "github:numtide/treefmt-nix";
    haskell-nix.url = "github:input-output-hk/haskell.nix/c0011f6eb3e83dbe48810c59c09b2b2b65ab16a9";
  };

  outputs =
    { self
    , nixpkgs
    , haskell-nix
    , flake-parts
    , treefmt
    , ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ treefmt.flakeModule ];
      perSystem = { config, pkgs, lib, system, ... }:
        let
          project = pkgs.haskell-nix.cabalProject' {
            name = "curl";
            src = self;
            compiler-nix-name = "ghc927";
            modules = [
              {
                config.packages.curl.components.library.libs =
                  lib.mkForce (
                    [
                      pkgs.curl
                      pkgs.openssl
                    ]
                  );
              }

            ];
            shell = rec {
              withHoogle = true;
              buildInputs = with pkgs;
                [
                  curl
                  openssl
                  haskellPackages.hlint
                  haskellPackages.fourmolu
                ]
                ++ builtins.attrValues config.treefmt.build.programs;
              tools = {
                haskell-language-server = "latest";
                cabal = "latest";
              };
              # This is needed so that `cabal repl` (which doesn't invoke GCC)
              # can find `libcurl.so`
              shellHook = ''
                export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}
              '';
            };
          };
        in
        rec {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            inherit (haskell-nix) config;
            overlays = [
              haskell-nix.overlays.combined
              (_: _: { curl-jr = project; })
            ];
          };

          inherit (pkgs.curl-jr.flake { }) packages checks;

          devShells.default = (pkgs.curl-jr.flake { }).devShell;

          # formatter = treefmt.lib.mkWrapper pkgs treefmt.config;

          treefmt.config = {
            projectRootFile = "flake.nix";
            programs = {
              nixpkgs-fmt.enable = true;
              cabal-fmt.enable = true;
              # For the cbits
              clang-format.enable = true;
              ormolu = {
                enable = true;
                ghcOpts = [ "TypeApplications" ];
              };
            };
          };
        };
    };
}
