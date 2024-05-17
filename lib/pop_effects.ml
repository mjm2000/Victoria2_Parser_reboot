open Lexer
open Exception
type pops_effect =
|CONSCIOUSNESS of int
|MILITANCY of int
|DOMINANT_ISSUE of string * int 
|IDEOLOGY of float * string 
|LITERACY of float
|MONEY of float 
|MOVE_ISSUE_PERCENTAGE of string * string * int 
|MOVE_POP of string |POP_TYPE of string
|REDUCE_POP of float 
|POP_EXCEPTION of exception_value
(*of exception_value*)
|LIMIT of Trigger.condition list

type ideology =
    |FACTOR of float
    |VALUE  of string
    |ERROR of (lexem_type * string * (int * int) )
    |EOF



let pops_effects effects =

let rec pops_effects_r effects out = match effects with
    |(KEYWORD,"limit",_)::(EQ,_,_)::(LB,_,_)::rest->
        let value,rest = Trigger.pop_condition rest in
        pops_effects_r rest (LIMIT(value)::out)

	|(KEYWORD,"consciousness",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (CONSCIOUSNESS((int_of_string v)))::out) 
	|(KEYWORD,"militancy",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (MILITANCY((int_of_string v)))::out) 
	|(KEYWORD,"dominant_issue",_)::(EQ,_,_)::(LB,_,_)
        ::(KEYWORD,"value",_)::(EQ,_,_)::(KEYWORD,value,_)
        ::(KEYWORD,"factor",_)::(EQ,_,_)::(INT,factor,_)::(LB,_,_)::rest->
		pops_effects_r rest ( (DOMINANT_ISSUE(value,(int_of_string factor)))::out) 
	|(KEYWORD,"ideology",_)::(EQ,_,_)::(LB,_,_)::rest->
            
        let rec ideology_mem ls out = match ls with
            |(KEYWORD,"factor",_)::(EQ,_,_)::(FLOAT,factor,_)::rest-> ideology_mem rest ((FACTOR (float_of_string factor))::out) 
            |(KEYWORD,"value",_)::(EQ,_,_)::(KEYWORD,value,_)::rest->
                
                ideology_mem rest ((VALUE value)::out) 
            |(RB,_,_)::rest -> 
                                out,rest
            |v::rest ->((ERROR v)::out),(v::rest)
            |[]->EOF::out,rest
        in
        let value,rest= match (ideology_mem rest []) with
            |(FACTOR factor)::(VALUE value)::[],rest -> IDEOLOGY (factor, value),rest
            |(VALUE value)::(FACTOR factor)::[],rest -> IDEOLOGY (factor, value),rest
            |_,(lexem,str,cords)::rest->POP_EXCEPTION(UNEXPECTED_LEXEM(str,lexem),cords),rest
            |_,[]->POP_EXCEPTION(END_OF_FILE,(0,0)),rest
        in
           

		pops_effects_r rest (value::out) 

	|(KEYWORD,"literacy",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (LITERACY((float_of_string v)))::out) 
	|(KEYWORD,"money",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (MONEY((float_of_string v)))::out) 
	|(KEYWORD,"move_issue_percentage",_)::(EQ,_,_)::(LB,_,_)::(KEYWORD,"from",_)::(EQ,_,_)::(KEYWORD,from,_)::(KEYWORD,"to",_)::(EQ,_,_)::(KEYWORD,to_v,_)::(KEYWORD,"value",_)::(EQ,_,_)::(INT,v,_)::(LB,_,_)::rest->
		pops_effects_r rest ( (MOVE_ISSUE_PERCENTAGE(from,to_v,(int_of_string v)))::out) 
	|(KEYWORD,"move_pop",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		pops_effects_r rest ( (MOVE_POP(v))::out) 
	|(KEYWORD,"pop_type",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		pops_effects_r rest ( (POP_TYPE(v))::out) 
	|(KEYWORD,"reduce_pop",_)::(EQ,_,_)::(FLOAT,v,_)::rest->
		pops_effects_r rest ( (REDUCE_POP((float_of_string v)))::out) 
    |(KEYWORD,"reduce_pop",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (REDUCE_POP((float_of_string v)))::out)

    |(RB,_,_)::rest->
         

            List.rev out,rest
    |(KEYWORD,str,cords)::(EQ,_,_)::_::rest->
        let exp_type = UNKNOWN_IDENTIFIER(str) in
        pops_effects_r rest (POP_EXCEPTION(exp_type,cords)::out ) 
    |(lexem,str,cords)::rest->
        let exp_type = UNEXPECTED_LEXEM(str,lexem) in
        pops_effects_r rest (POP_EXCEPTION(exp_type,cords)::out ) 

    |rest->out,rest
in
pops_effects_r effects []
