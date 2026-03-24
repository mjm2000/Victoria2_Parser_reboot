
nix-shell ../shell.nix --run "dune build"
nix-shell ../shell.nix --run 'dune exec ./main.exe -- --mp  -h "../test_data/victoria2" -m "../test_data/GFM" -g Victoria2 -o output.txt -a "ast.txt" -l "lex.txt" --mf ../test_data/GFM.mod'
#git add run.sh main.ml dune ../lib
#git commit -m "update"

