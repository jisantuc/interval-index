{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc96";
          haskellPackages = pkgs.haskell.packages.${compiler};
          packageDependencies = (ps: [
            ps.containers
            ps.deepseq
            ps.vector
          ]);
          devDependencies = with haskellPackages; [
            cabal-fmt
            cabal-install
            haskell-language-server
            hls-retrie-plugin
            hlint
            ormolu
          ];
          testDependencies = (ps: [
            ps.hspec
            ps.hspec-discover
          ]);
          benchDependencies = (ps: [
            ps.aeson
            ps.bytestring
            ps.criterion
          ]);
          haskell = haskellPackages.ghcWithPackages
            (ps: packageDependencies ps ++ testDependencies ps ++ benchDependencies ps);
        in
        {
          devShells.default = pkgs.mkShell
            {
              packages = [ haskell ] ++ devDependencies;
            };
          devShells.ci = pkgs.mkShell
            {
              packages = [ haskell ] ++ [
                haskellPackages.cabal-install
              ];
            };

          packages.default = haskellPackages.callCabal2nix "interval-index" ./. { };
        }
      );
}

