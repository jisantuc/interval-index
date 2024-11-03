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
          python = pkgs.python311.withPackages
            (p: with p; [
              black
              intervaltree
              isort
              pyperf
            ]);
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
            python
          ];
          testDependencies = (ps: [
            ps.hspec
            ps.hspec-discover
          ]);
          benchDependencies = (ps: [
            ps.aeson
            ps.criterion
            ps.random
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

