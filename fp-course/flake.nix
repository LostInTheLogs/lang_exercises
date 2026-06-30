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

      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
      in {
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [];
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
