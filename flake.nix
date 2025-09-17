{
  description = "Utility to manage GitHub workflows for a user";

  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with import nixpkgs {
        inherit system;
        overlays = [ (import ./haskell-overrides.nix) ];
      };
      let
        packages =
          lib.attrsets.genAttrs
            [ "ghc810" "ghc92" "ghc94" "ghc96" "ghc98" "ghc910" "ghc912" ]
            (compiler: haskell.packages.${compiler}.callPackage ./. { });
        devShells =
          lib.attrsets.mapAttrs
            (_: p: p.env or p)
            packages;
      in
      {
        packages = packages // {
          default = packages.ghc910;
        };
        devShells = devShells // {
          default = devShells.ghc910;
        };
      });
}
