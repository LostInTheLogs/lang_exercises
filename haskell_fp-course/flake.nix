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

      flake = {
        overlay = final: prev: {
          # Attach directly to the top-level pkgs scope
          haskell-app = final.haskell.packages.ghc984.callCabal2nix "haskell-app" ./. {};
        };
      };

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
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            self.overlay
          ];
          config = {};
        };

        packages = {
          haskell-app = pkgs.haskell-app;
          default = pkgs.haskell-app;
        };

        checks = self'.packages;

        devShells.default = hp.shellFor {
          # Reference pkgs.haskell-app here since overlay put it on top-level pkgs
          packages = p: [pkgs.haskell-app];
          withHoogle = true;
          buildInputs = [
            (pkgs.haskell-language-server.override {
              supportedGhcVersions = [
                "984"
              ];
            })
            # pkgs.dap
            hp.ghcid
            hp.cabal-install
            pkgs.fourmolu
          ];
        };
      };
    };
}
