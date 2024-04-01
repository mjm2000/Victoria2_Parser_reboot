open Lexer
open Exception
type pops_effect =
|CONSCIOUSNESS of int
|MILITANCY of int
|DOMINANT_ISSUE of string * int 
|IDEOLOGY of int * string 
|LITERACY of int
|MONEY of int
|MOVE_ISSUE_PERCENTAGE of string * string * int 
|MOVE_POP of string |POP_TYPE of string
|REDUCE_POP of int
|POP_EXCEPTION of exception_value


let pops_effects effects =

let rec pops_effects_r effects out = match effects with
	|(KEYWORD,"consciousness",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (CONSCIOUSNESS((int_of_string v)))::out) 
	|(KEYWORD,"militancy",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (MILITANCY((int_of_string v)))::out) 
	|(KEYWORD,"dominant_issue",_)::(EQ,_,_)::(LB,_,_)
        ::(KEYWORD,"value",_)::(EQ,_,_)::(KEYWORD,value,_)
        ::(KEYWORD,"factor",_)::(EQ,_,_)::(INT,factor,_)::(LB,_,_)::rest->
		pops_effects_r rest ( (DOMINANT_ISSUE(value,(int_of_string factor)))::out) 
	|(KEYWORD,"ideology",_)::(EQ,_,_)::(LB,_,_)::(KEYWORD,"factor",_)::(EQ,_,_)::(INT,factor,_)::(KEYWORD,"value",_)::(EQ,_,_)::(KEYWORD,value,_)::(LB,_,_)::rest->
		pops_effects_r rest ( (IDEOLOGY(int_of_string factor,value))::out) 
	|(KEYWORD,"literacy",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (LITERACY((int_of_string v)))::out) 
	|(KEYWORD,"money",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (MONEY((int_of_string v)))::out) 
	|(KEYWORD,"move_issue_percentage",_)::(EQ,_,_)::(LB,_,_)::(KEYWORD,"from",_)::(EQ,_,_)::(KEYWORD,from,_)::(KEYWORD,"to",_)::(EQ,_,_)::(KEYWORD,to_v,_)::(KEYWORD,"value",_)::(EQ,_,_)::(INT,v,_)::(LB,_,_)::rest->
		pops_effects_r rest ( (MOVE_ISSUE_PERCENTAGE(from,to_v,(int_of_string v)))::out) 
	|(KEYWORD,"move_pop",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		pops_effects_r rest ( (MOVE_POP(v))::out) 
	|(KEYWORD,"pop_type",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		pops_effects_r rest ( (POP_TYPE(v))::out) 
	|(KEYWORD,"reduce_pop",_)::(EQ,_,_)::(INT,v,_)::rest->
		pops_effects_r rest ( (REDUCE_POP((int_of_string v)))::out) 

    |(RB,_,_)::rest->out,rest
    |(KEYWORD,str,cords)::(EQ,_,_)::_::rest->
        let exp_type = UNKNOWN_IDENTIFIER(str) in
        pops_effects_r rest (POP_EXCEPTION(exp_type,cords)::out ) 
    |(lexem,str,cords)::rest->
        let exp_type = UNEXPECTED_LEXEM(str,lexem) in
        pops_effects_r rest (POP_EXCEPTION(exp_type,cords)::out ) 

    |rest->out,rest
in
pops_effects_r effects []
