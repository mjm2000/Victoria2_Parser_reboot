{
  description = "Victoria 2 parser";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ocamlPkgs = pkgs.ocamlPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; with ocamlPkgs; [
            zsh
            ocaml
            dune_3
            findlib
            cmdliner
            utop
            merlin
            ocaml-lsp
            ocamlformat
            re2
            containers
            alcotest
            git
          ];
        };

        apps.default = {
          type = "app";
          program = toString (pkgs.writeShellScript "run" ''
            if [ -f "./bin/run.sh" ]; then
              cd "./bin"
              exec bash "./run.sh"
            elif [ -f "${self}/bin/run.sh" ]; then
              cd "${self}/bin"
              exec bash "${self}/bin/run.sh"
            else
              echo "run.sh not found; run this command from the project root" >&2
              exit 1
            fi
          '');
        };
      });
}
