
nix-shell ../shell.nix --run "dune build"
nix-shell ../shell.nix --run 'dune exec ./main.exe -- --mp  -h "../test_data/eu4" -m "/home/jimmy/projects/victoria_2_parser/test_data/divided_colonies" -g Eu4 -o output.txt -a "ast.txt" -l "lex.txt"'
#git add run.sh main.ml dune ../lib
#git commit -m "update"

