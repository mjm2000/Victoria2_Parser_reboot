
nix-shell ../shell.nix --run "dune build"
nix-shell ../shell.nix --run 'dune exec ./main.exe -- --mp  -h "../test_data/victoria2" -m "../test_data/TGCHFMMap" -g Victoria2 -o output.txt -a "ast.txt" -l "lex.txt"'
#git add run.sh main.ml dune ../lib
#git commit -m "update"

