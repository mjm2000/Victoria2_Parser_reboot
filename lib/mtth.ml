open Lexer
open Trigger
open Trigger_string

type event_type = PROVINCE | COUNTRY
type time = 
    |DAY of int
    |MONTH of int |YEAR of int |MODIFIER of string * condition list
    |FACTIOR_EXCEPTION of int * int
    |INVALID_TOKEN of lexem_type * string * int * int
    |EOF of int * int
let mtth lexems scope = 
    let rec mtth_r lexems scope out = 
        match lexems with
            |(KEYWORD,"year",_)::(EQ,_,_)::(INT,i,_)::rest->
                let x = YEAR(int_of_string i) in
                mtth_r rest scope (x::out) 
            |(KEYWORD,"months",_)::(EQ,_,_)::(INT,i,_)::rest->
                let x = MONTH(int_of_string i) in
                mtth_r rest scope (x::out)
            |(KEYWORD,"days",_)::(EQ,_,_)::(INT,i,_)::rest->
                let x = MONTH(int_of_string i) in
                mtth_r rest scope (x::out)                 
            |(KEYWORD,"modifier",(x,y))::(EQ,_,_)::(LB,_,_)::rest->
                let factor,rest =  match rest with
                |(KEYWORD,"factor",_)::(EQ,_,_)::(INT,v,_)::ls
                |(KEYWORD,"factor",_)::(EQ,_,_)::(FLOAT,v,_)::ls-> 
                       Some v,ls
                |_-> None ,rest
                in
                let cond,rest = match scope with 
                    |COUNTRY-> country_conditions rest 
                    |PROVINCE-> province_conditions rest
                in 
                (match factor with 
                |Some v ->  
                    mtth_r rest scope (MODIFIER(v, cond):: out)
                |None ->
                    mtth_r rest scope (FACTIOR_EXCEPTION(x,y) :: out)
                )
            |(RB,_,_)::rest-> List.rev out,rest
            |(_,_,(x,y))::[]-> EOF(x,y)::out,[]
            |(l,value,(x,y))::rest->List.rev (INVALID_TOKEN(l,value,x,y)::out),rest
            |[]->out,[]
in
    mtth_r lexems scope []

let mtth_string time = match time with 
    |YEAR(d) -> Printf.sprintf "year:%i" d 
    |MONTH(d) -> Printf.sprintf "month:%i" d
    |DAY(d) -> Printf.sprintf "day:%i" d
    |MODIFIER(f,values) -> Printf.sprintf "modifier:{\n\tfactor=%s\n%s\t}\n" f (triggers_string values)
    |FACTIOR_EXCEPTION(x,y) ->Printf.sprintf "Invalid Factor:%i,%i" x y
    |INVALID_TOKEN(l,s,x,y)-> 
            Printf.sprintf "Invalid token (text:%s:type:%s):%i,%i" (lexem_to_str l) s x y
    |EOF(x,y)-> Printf.sprintf ("EOF:%i,%i") x y

