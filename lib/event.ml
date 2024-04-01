open Country_effects
open Province_effects
open Effect_string
open Trigger
open Trigger_string
open Lexer
open Mtth
type event_items = 
    |SYNTAX_ERROR of lexem_type  * lexem_type * string * int * int 
    |ID of int 
    |TITLE of string
    |DESC of string
    |PICTURE of string
    |ALLOW_MULTIPLE_INSTANCES of bool
    |FIRE_ONLY_ONCE of bool
    |IS_TRIGGERED_ONLY of bool
    |COUNTRY_IMMEDIATE of country_effect list
    |PROVINCE_IMMEDIATE of province_effect list
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
    |MTTH of Mtth.time list    
    |COUNTRY_OPTION of string * country_effect list
    |PROVINCE_OPTION of string * province_effect list
    |MISSING_BRACKET
    |NAME_ERROR of int * int
    |END_OF_FILE of int * int


let print_event_item event_item = 
    match event_item with
    |ID(id) -> Printf.printf "id: %i\n" id;
    |TITLE(value) -> Printf.printf "title: %s\n" value;

    |DESC(value) ->  Printf.printf "dest: %s\n" value;

    |MAJOR(value) -> Printf.printf "major: %b\n" value;
    |IS_TRIGGERED_ONLY(value) -> Printf.printf "is_triggered_only: %b\n" value;
    |COUNTRY_TRIGGER(values) ->  
        Printf.printf "Country Trigger {\n";
        print_trigger values;
        Printf.printf "}\n";
    |PROVINCE_TRIGGER(values) ->  
        Printf.printf "Province Trigger {\n";
        print_trigger values;
        Printf.printf "}\n";
    |COUNTRY_IMMEDIATE(values) -> 
        Printf.printf "Country Immediate {\n";

        List.iter (fun a -> Printf.printf "\t%s\n" (country_effect_string a) ) values ;
        Printf.printf "}\n";
    |PROVINCE_IMMEDIATE(values) -> 
        Printf.printf "Province Immediate {\n";

        List.iter (fun a -> Printf.printf "\t%s\n" (province_effect_string a) ) values ;
        Printf.printf "}\n";
    |COUNTRY_OPTION(id,values)->
        Printf.printf "Country Option: %s{\n" id ;

        List.iter (fun x -> Printf.printf "\t%s\n"  (country_effect_string x)) values;
        Printf.printf "}\n";
    |PROVINCE_OPTION(id,values)->
        Printf.printf "Province Option: %s{\n" id ;
        List.iter (fun x -> Printf.printf "\t%s\n"  (province_effect_string x)) values;
        Printf.printf "}\n";
    |MTTH(times)->

        Printf.printf "MTTH: {\n";
        List.iter (fun x -> Printf.printf "\t%s\n" (Mtth.mtth_string x)) times;
        Printf.printf "}\n";

    
    
    |_->Printf.printf ""

let print_event event= List.iter print_event_item event  
    
let print_events events = List.iter print_event events  


let event lexems event_type =

let rec read_event_body lexems out = 
   (* let (_,v,_)=(List.hd lexems) in Printf.printf "nigger:%s\n" v; *)
    match lexems with 
    |(RB,_,_)::ls ->List.rev out,ls
    |(KEYWORD,("id"),_)::(EQ,_,_)::(INT, value,_)::rest -> 
        let v = ID(int_of_string value) in 
        read_event_body  rest (v::out)
    |(KEYWORD,("id"),_)::(EQ,_,_)::(type_value, value,(x,y))::rest -> 
        let v = SYNTAX_ERROR(INT,type_value,value,x,y) in 
        read_event_body  rest (v::out) 
    |(KEYWORD,("title"),_)::(EQ,_,_)::(STRING,value,_)::rest -> 
        let v = TITLE ( value) in 
        read_event_body  rest (v::out)

    |(KEYWORD,("title"),_)::(EQ,_,_)::(type_value,value,(x,y))::rest ->
        let v = SYNTAX_ERROR(STRING,type_value,value,x,y) in 
        read_event_body  rest (v::out)
    |(KEYWORD,("picture"),_)::(EQ,_,_)::(STRING,value,_)::rest -> 
        let v = PICTURE ( value) in 
        read_event_body  rest (v::out)
                                                                        
    |(KEYWORD,("picture"),_)::(EQ,_,_)::(type_value,value,(x,y))::rest ->
    let v = SYNTAX_ERROR(STRING,type_value,value,x,y) in 
    read_event_body  rest (v::out)
    |(KEYWORD,("desc"),_)::(EQ,_,_)::(STRING,value,_)::rest -> 
        let v = DESC ( value) in 
        read_event_body  rest (v::out)
    |(KEYWORD,("desc"),_)::(EQ,_,_)::(type_value,value,(x,y))::rest -> 
        let v = SYNTAX_ERROR(STRING,type_value,value,x,y) in 
        read_event_body  rest (v::out)
    |(KEYWORD,("major"),_)::(EQ,_,_)::(BOOL,value,_)::rest -> 
        
        let v = MAJOR(bool_of_string (value)) in 
        read_event_body  rest (v::out)
    |(KEYWORD,("major"),_)::(EQ,_,_)::(type_value,value,(x,y))::rest -> 
        let v = SYNTAX_ERROR(BOOL,type_value,value,x,y) in 
        read_event_body  rest (v::out)
    |(KEYWORD,("is_triggered_only"),_)::(EQ,_,_)::(BOOL,value,_)::rest -> 
        let v = IS_TRIGGERED_ONLY(bool_of_string (value) ) in 
        read_event_body  rest (v::out)
    |(KEYWORD,("is_triggered_only"),_)::(EQ,_,_)::(type_value,value,(x,y))::rest -> 
        let v = SYNTAX_ERROR(BOOL,type_value,value,x,y) in 
        read_event_body  rest (v::out)
    |(KEYWORD,("trigger"),_)::(EQ,_,_)::(LB,_,_)::rest -> 
        let v,rest = match event_type with  
        |COUNTRY ->  let x,rest =country_conditions rest in
            COUNTRY_TRIGGER(x),rest
        |PROVINCE ->  let x,rest = (province_conditions rest) in
            PROVINCE_TRIGGER(x),rest
        in
        read_event_body  rest (v::out) 
    |(KEYWORD,"immediate",_)::(EQ,_,_)::(LB,_,_)::rest -> 
         let v,rest = match event_type with  
        |COUNTRY ->  
            let x,rest = Country_effects.country_effects rest in
            COUNTRY_IMMEDIATE(x),rest
        |PROVINCE ->  let x,rest = (province_effects rest) in
            PROVINCE_IMMEDIATE(x),rest
        in
        read_event_body  rest (v::out) 
    |(KEYWORD,"mean_time_to_happen",_)::(EQ,_,_)::(LB,_,_)::rest -> 
        let mtth,rest = Mtth.mtth rest event_type in 
        let v = MTTH(mtth) in
        Printf.printf "MEAN TIME\n";
        read_event_body  rest (v::out)
    |(KEYWORD,"option",_)::(EQ,_,_)::(LB,_,_)::rest -> 
        let v,rest = match rest with
        |(KEYWORD,"name",_)::(EQ,_,_)::(STRING,v,_)::rest ->
           (match event_type with  
           |COUNTRY ->  let x,rest = country_effects rest in
                Printf.printf "%i:values\n" (List.length x);
               COUNTRY_OPTION(v,x),rest
           |PROVINCE ->  let x,rest = (province_effects rest) in
               PROVINCE_OPTION(v,x),rest
            )
        |(_,_,(x,y))::[]->END_OF_FILE(x,y),[]
        |(_,_,(x,y))::rest-> NAME_ERROR(x,y),rest
        |[]->MISSING_BRACKET,[]
        in 

        read_event_body  rest (v::out)
    |(_,_,(x,y))::[]->
        let v = END_OF_FILE(x,y) in 
        read_event_body  [] (v::out)
    |(type_value,value,(x,y))::rest->  
        let v = SYNTAX_ERROR(RB,type_value,value,x,y) in 
        read_event_body  rest (v::out)  
    |rs->(List.rev out),rs

in  
    (read_event_body lexems [])

let read_events lexems =   
    let rec read_event_r lexems out =  
        match lexems with
        |(KEYWORD,("province_event"),_)::(EQ,_,_)::(LB,_,_)::ls -> 

            let event_value,rest = event ls PROVINCE in
            read_event_r rest (event_value::out)  
        |(KEYWORD,("country_event"),_)::(EQ,_,_)::(LB,_,_)::ls -> 

            let event_value,rest = event ls COUNTRY in
            read_event_r rest (event_value::out)  

        |_-> 
                out 
    in
    List.rev(read_event_r lexems [])

