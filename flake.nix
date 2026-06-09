{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.11";
  };

  outputs = {nixpkgs, ...} @ inputs: let
    inherit (nixpkgs) lib;

    eachSystem = f:
      nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (
        system: f system nixpkgs.legacyPackages.${system}
      );

    beamVersion = "beam28Packages";
  in {
    packages = eachSystem (_system: pkgs: {});

    devShells = eachSystem (
      _system: pkgs: {
        default = pkgs.mkShell {
          buildInputs = [
            pkgs.gleam
            pkgs.${beamVersion}.erlang
            pkgs.nodejs
          ];
        };
      }
    );
  };
}
