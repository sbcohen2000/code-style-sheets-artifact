{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          devShell = with pkgs; mkShell {
            buildInputs = [
              cabal-install
              ghc
              haskell-language-server
              zlib
              nodejs_22
            ];
          };
        }
      );
}