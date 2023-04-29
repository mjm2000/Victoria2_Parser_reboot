open Types
type time_unit = 
    | YEAR of int 
    | MONTH of int 
    | DAY of int 
    | Modifier 
type country_cond = 
    |TIME of time_unit
    |NOT of country_cond list 
    |AND of country_cond list 
    |OR of country_cond list
    |ALL_CORE of country_cond list
    |ANY_CORE of country_cond list
    |ANY_GREATER_POWER of country_cond list
    |ANY_NEIGHBOR_COUNTRY of country_cond list
    |ANY_OWNED_PROVINCE of country_cond list
    |ANY_POP of country_cond list
    |ANY_SPHERE_MEMBER of country_cond list
    |ANY_STATE of country_cond list
    |ANY_SUBSTATE of country_cond list
    |CAPITAL_SCOPE of country_cond list
    |CULTURAL_UNION of country_cond list
    |OVERLORD of country_cond list
    |SPHERE_OWNER of country_cond list
    |WAR_COUNTRIES of country_cond list
    |COUNTRY_TAG of string * country_cond list
    |REGION_NAME of string * country_cond list
    | MAJOR

   

type province_cond = 
    |NOT of province_cond list 
    |AND of province_cond  list 
    |OR of province_cond list
    |ANY_CORE of province_cond list
    |ANY_NEIGHBOR_PROVINCE of province_cond list
    |ANY_POP of province_cond list
    |CONTROLLER of province_cond list
    |OWNER of province_cond list
    |STATE_SCOPE of province_cond list
(*
type pop_cond 
    location
    country
    cultural_union
*)
type country_effect = 
    |Prestige of int 
type province_effect = 
    |Prestige of int 

type mtth = TIME of time_unit 



type event_items = 
    |SYNTAX_ERROR of string 
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
    |NEWS of bool 
    |NEWDS of string 
    |NEWSDM of string 
    |NEWSDL of string 
    |COUNTRY_TRIGGER of country_cond list 
    |PROVINCE_TRIGGER of province_cond list
    |MTTH of time_unit list    
    |COUNTRY_OPTION of string * country_effect list
    |PROVINCE_OPTION of string * province_effect list


let handle_type required_type received_type =  
    match received_type,required_type with
    |Float(_) ,   "float"   
    |Int(_) ,     "int" 
    |String(_) ,  "string" 
    |Bool(_) ,    "bool" 
    |Keyword(_) , "keyword" 
    |FROM ,       "from" 
    |THIS ,       "this" 
    |Tag(_) ,     "tag"-> None  
    |v,type_val   ->   
            Some (Printf.sprintf "Expected type:%s Received %s" (Lexer.lexem_to_str v) type_val)





type event_type = PROVINCE | COUNTRY
let rec read_event_body lexems  out event_type = 
    match lexems with 
    |(RB,_)::ls -> out,ls
    | (Keyword("id"),_)::(EQ,_)::(value,(x2,y2))::rest -> 
         let expr = match (handle_type "int" value ) with  
        |Some syntax_error -> SYNTAX_ERROR(Printf.sprintf "Error %i:%i: %s" x2 y2 syntax_error) 
        |None -> ID(int_of_string(Lexer.lexem_to_value value) ) 
        in
        read_event_body  rest (expr::out) event_type
   
    |_-> out,lexems


let read_events lexems =  
    let rec read_event_r lexems out =  
        match lexems with
        |(Keyword("province_event"),_)::(EQ,_)::(LB,_)::ls -> 

                let event,rest = read_event_body ls [] PROVINCE in
                read_event_r rest (event::out)  
        |(Keyword("country_event"),_)::ls -> 
                let event,rest = read_event_body ls [] COUNTRY in
                    read_event_r rest (event::out)  

        |_->out
    in
    read_event_r lexems [] 
