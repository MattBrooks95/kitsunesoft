{
  description = "frontend for fsp spreadsheets";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system :
  let pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.default = pkgs.mkShell {
      buildInputs = [
        pkgs.nodejs_20
        pkgs.nodePackages.purescript-language-server
      ];
    };
  });
}
