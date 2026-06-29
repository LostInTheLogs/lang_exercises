{
  description = "A Hello World in Haskell with a dependency and a devShell (using flake-parts)";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
      systems = ["x86_64-linux" "x86_64-darwin"];

      flake = {
        overlay = final: prev: {
          haskell-app = final.haskellPackages.callCabal2nix "haskell-app" ./. {};
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
      in {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            self.overlay
          ];
          config = {};
        };

        packages = {
          haskell-app = pkgs.haskellPackages.haskell-app;
          default = pkgs.haskell-app;
        };

        checks = self'.packages;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [pkgs.haskell-app];
          withHoogle = true;
          buildInputs = with pkgs.haskellPackages; [
            haskell-language-server
            dap
            ghcid
            cabal-install
          ];
        };
      };
    };
}
