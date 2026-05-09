
nix-shell ../shell.nix --run "dune build"
nix-shell ../shell.nix --run 'dune exec ./main.exe -- --mp  -h "../testdata/Europa Universalis IV" -m "/home/jimmy/projects/victoria_2_parser/testdata/Paradox Interactive/Europa Universalis IV/mod/divided_colonies" -g Eu4 -o output.txt  --mf "/home/jimmy/projects/victoria_2_parser/testdata/Paradox Interactive/Europa Universalis IV/mod/div.mod"'
#git add run.sh main.ml dune ../lib
#git commit -m "update"

