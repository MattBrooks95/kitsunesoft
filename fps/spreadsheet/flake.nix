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
        pkgs.purescript
        pkgs.spago
      ];
      # TODO this is so that spago can find esbuild....
      # it uses haskell's 'findExecutable' function to get ahold of esbuild
      # but that looks at the path, and not the node packages for the project
      # doing it this way means that you need to be in the same directory as this
      # flake for the PATH entry for esbuild to make sense
      shellHook = ''
      export PATH="$PATH:node_modules/esbuild/bin"
      '';
    };
  });
}
