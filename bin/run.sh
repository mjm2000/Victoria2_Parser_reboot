dune build
dune exec ./main.exe -- -h "../victoria2" -m "../The-Grand-Combo/TGC" -g Victoria2 -o output.txt
git add run.sh main.ml dune ../lib
git commit -m "update"
