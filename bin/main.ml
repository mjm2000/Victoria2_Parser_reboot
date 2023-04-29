open Vic2parser_reboot
open Lexer
let () = print_endline "Hello, World!";
lexer "/home/jimmy/projects/vic2parser_reboot/HFM/HFM/events/ACW.txt" |> print_lexems
