open Vic2parser_reboot 
open Lexer

let () = 
    lexer (Array.get  Sys.argv 1) 
    (*|> Lexer.print_lexems *)
    |> Event.read_events 
    |> Event.print_events
    ;
