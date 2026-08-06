{
  description = "A Hello World in Haskell with a dependency and a devShell (using flake-parts)";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-26.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"];

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        hp = pkgs.haskell.packages.ghc984;
      in {
        packages = {
          default = hp.callCabal2nix "hgit" ./. {};
        };

        checks = self'.packages;

        devShells.default = hp.shellFor {
          # Reference pkgs.hgit here since overlay put it on top-level pkgs
          packages = p: [
            self'.packages.default
          ];
          withHoogle = true;
          buildInputs = [
            (pkgs.haskell-language-server.override {
              supportedGhcVersions = [
                "984"
              ];
            })
            hp.hpack
            pkgs.haskellPackages.cabal-install
            # pkgs.dap
          ];
        };
      };
    };
}
