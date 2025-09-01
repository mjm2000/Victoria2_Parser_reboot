{
  description = "A development shell for my OCaml project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ocamlPackages.dune_3 # Or your desired version
            ocaml
            ocamlPackages.cmdliner
            # Add any other libraries or tools here
            # e.g., ocamlPackages.utop, ocamlPackages.ocamlformat
          ];
        };
      });
}
