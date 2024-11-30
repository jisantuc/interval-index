{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc98";
          haskellPackages = pkgs.haskell.packages.${compiler};
          devDependencies = with haskellPackages; [
            cabal-fmt
            cabal-install
            haskell-language-server
            hlint
            ormolu
            python
          ];
        in
        {
          devShells.default = haskellPackages.shellFor {
            packages = ps: [ (ps.callCabal2nix "interval-index" ./. { }) ];
            nativeBuildInputs = devDependencies;
            withHoogle = true;
          };
          devShells.ci = haskellPackages.shellFor {
            packages = ps: [ (ps.callCabal2nix "interval-index" ./. { }) ];
            nativeBuildInputs = with haskellPackages; [
              cabal-install
            ];
          };

          packages.default = haskellPackages.callCabal2nix "interval-index" ./. { };
        }

      );
}

