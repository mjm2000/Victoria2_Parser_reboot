dune build
dune exec ./main.exe -- -h "../victoria2" -m "../HFM" -g Victoria2 -o output.txt
git add run.sh main.ml dune ../lib
git commit -m "update"
