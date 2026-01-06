{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  
  
  
  
  buildInputs = [
    pkgs.zsh
    pkgs.ocamlPackages.ocaml
    pkgs.ocamlPackages.dune_3
    pkgs.ocamlPackages.findlib
    pkgs.ocamlPackages.cmdliner
    pkgs.ocamlPackages.utop
    pkgs.ocamlPackages.merlin
    pkgs.ocamlPackages.ocaml-lsp
    pkgs.ocamlPackages.ocamlformat
    pkgs.ocamlPackages.re2
    pkgs.ocamlPackages.containers
    pkgs.ocamlPackages.alcotest
    pkgs.git
  ];




}
