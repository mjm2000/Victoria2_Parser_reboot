
nix-shell ../shell.nix --run "dune build"
nix-shell ../shell.nix --run 'dune exec ./main.exe -- --mp  -h "/home/jimmy/.local/share/Steam/steamapps/common/Victoria 2/mod/NL_GFM" -m ".." -g Victoria2 -o output.txt -a "ast.txt" -l "lex.txt" --mf "/home/jimmy/.local/share/Steam/steamapps/common/Victoria 2/mod/NL_GFM.mod"'
#git add run.sh main.ml dune ../lib
#git commit -m "update"

