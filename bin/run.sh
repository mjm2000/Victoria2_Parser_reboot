dune build
dune exec ./main.exe -- -h "../victoria2" -m "../HFM/HFM" -g Victoria2 -o output.txt -a "ast.txt" -l "lex.txt"
git add run.sh main.ml dune ../lib
git commit -m "update"
