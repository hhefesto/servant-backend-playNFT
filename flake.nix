{
  description = "Nix flake for servant backend of playNFT";

  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          servant-backend-playnft = final.haskell-nix.cabalProject {
            # If these null parameters are absent, you get a RestrictedPathError error
            # from trying to do readIfExists on cabal.project file
            cabalProjectFreeze = null;
            cabalProject = null;
            cabalProjectLocal = null;

            src = final.haskell-nix.cleanSourceHaskell {
              src = ./.;
              name = "servant-backend-playnft";
            };
            # compiler-nix-name = "ghc884";
            compiler-nix-name = "ghc8107";
            # compiler-nix-name = "ghc921";
            # This is used by `nix develop .` to open a shell for use with
            # `cabal`, `hlint` and `haskell-language-server`
            shell.tools = {
              cabal = {};
              haskell-language-server = {};
            };
          };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.servant-backend-playnft.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."servant-backend-playnft:exe:servant-backend-playnft";
      defaultApp = flake.apps."servant-backend-playnft:exe:servant-backend-playnft";
      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
    });
}
