
nix-shell ../shell.nix --run "dune build"
nix-shell ../shell.nix --run 'dune exec ./main.exe -- --mp  -h "../testdata/victoria2" -m "../testdata/victoria2/mod/GFM" -g Victoria2 -o output.txt -a "ast.txt" -l "lex.txt" --mf "../testdata/victoria2/mod/GFM.mod"'
#git add run.sh main.ml dune ../lib
#git commit -m "update"

