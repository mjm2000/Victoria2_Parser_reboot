dune build
dune exec ./main.exe -- -h "../victoria2" -m "../test_data/DOD/" -g Victoria2 -o output.txt -a "ast.txt" -l "lex.txt"
git add run.sh main.ml dune ../lib
git commit -m "update"
