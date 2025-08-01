{
  description = "Utilities to query GitHub repositories for a user";

  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    let
      compiler = "ghc910";
    in
    flake-utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; };
      with self.packages.${system};
      {
        packages.default = haskell.packages.${compiler}.callPackage ./. { };
        devShells.default = default.env;
      }
    );
}
