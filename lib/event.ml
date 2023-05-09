open Types
open Trigger
type time_unit = 
    | YEAR of int 
    | MONTH of int 
    | DAY of int 
    | Modifier 




type country_effect = 
    |Prestige of int 
type province_effect = 
    |Prestige of int 

type mtth = TIME of time_unit 


type event_items = 
    |SYNTAX_ERROR of int * int * lexem_type * lexem_type
    |ID of int 
    |TITLE of string
    |DESC of string
    |PICTURE of string
    |ALLOW_MULTIPLE_INSTANCES of bool
    |FIRE_ONLY_ONCE of bool
    |IS_TRIGGERED_ONLY of bool
    |COUNTRY_IMMEDIATE of bool
    |PROVINCE_IMMEDIATE of bool
    |CHECK_VARIABLE of bool
    |HAS_GLOBAL_FLAG of bool
    |IS_CANAL_ENABLED of bool
    |MAJOR of bool
    |NEWS of bool 
    |NEWDS of string 
    |NEWSDM of string 
    |NEWSDL of string 
    |COUNTRY_TRIGGER of condition  list 
    |PROVINCE_TRIGGER of condition list
    |MTTH of time_unit list    
    |COUNTRY_OPTION of string * country_effect list
    |PROVINCE_OPTION of string * province_effect list
type event_type = PROVINCE | COUNTRY



let event lexems event_type =
let rec read_event_body lexems out = 
    match lexems with 
    |(RB,_,_)::ls -> out,ls
    |(KEYWORD,("id"),_)::(EQ,_,_)::(INT, value,(x,y))::rest -> 
        let v = ID(int_of_string value) in 
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("id"),_)::(EQ,_,_)::(type_value, value,(x,y))::rest -> 
        let v = SYNTAX_ERROR(x,y,INT,type_value) in 
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("title"),_)::(EQ,_,_)::(STRING,value,(x,y))::rest -> 
        let v = TITLE ( value) in 
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("title"),_)::(EQ,_,_)::(type_value,value,(x,y))::rest ->
        let v = SYNTAX_ERROR (x,y,STRING,type_value) in 
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("desc"),_)::(EQ,_,_)::(STRING,value,(x,y))::rest -> 
        let v = DESC ( value) in 
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("desc"),_)::(EQ,_,_)::(type_value,value,(x,y))::rest -> 
        let v = SYNTAX_ERROR (x,y,STRING,type_value) in
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("major"),_)::(EQ,_,_)::(STRING,value,(x,y))::rest -> 
        let v = MAJOR(bool_of_string (value)) in 
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("major"),_)::(EQ,_,_)::(type_value,value,(x,y))::rest -> 
        let v = SYNTAX_ERROR (x,y,BOOL,type_value) in
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("is_triggered_only"),_)::(EQ,_,_)::(BOOL,value,(x,y))::rest -> 
        let v = IS_TRIGGERED_ONLY(bool_of_string (value) ) in 
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("is_triggered_only"),_)::(EQ,_,_)::(type_value,value,(x,y))::rest -> 
        let v = SYNTAX_ERROR (x,y,BOOL,type_value) in
        read_event_body  rest (v::out) event_type
    |(KEYWORD,("trigger"),_)::(EQ,_,_)::(LB,_,_)::rest -> 
        let v = TRIGGER (bool_of_string (value)) in 
        read_event_body  rest (v::out) event_type
    |_-> out,lexems
in  
    read_event_body lexems [] 

let read_events lexems =   
    let rec read_event_r lexems out =  
        match lexems with
        |(KEYWORD,("province_event"),_)::(EQ,_,_)::(LB,_)::ls -> 
            let event,rest = read_event_body ls [] PROVINCE in
            read_event_r rest (event::out)  
        |(KEYWORD,("country_event"),_)::ls -> 
            let event,rest = read_event_body ls [] COUNTRY in
            read_event_r rest (event::out)  

        |_->out 
    in
    read_event_r lexems [] 
