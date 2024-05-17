open Lexer
type pop_type  = 
    |ANY_POP 
    |ARISTOCRATS
    |ARTISANS
    |BUREAUCRATS
    |CAPITALISTS
    |CLERGYMEN
    |CLERKS
    |CRAFTSMEN
    |FARMERS
    |LABOURERS
    |OFFICERS
    |SLAVES
    |SOLDIERS
    |POOR_STRATA
    |MIDDLE_STRATA
    |RICH_STRATA

type province_effect =
|SET_GLOBAL_FLAG of string
|CLR_GLOBAL_FLAG of string
|ASSIMILATE of bool 
|ADD_CORE of string
|ADD_PROVINCE_MODIFIER of string * int 
|CHANGE_CONTROLLER of string
|FORT of int
|INFRASTRUCTURE of int
|LIFE_RATING of int
|NAVAL_BASE of int
|REMOVE_CORE of string
|REMOVE_PROVINCE_MODIFIER of string
|RGO_SIZE of int
|SECEDE_PROVINCE of string
|SUB_UNIT of string * string
|TRADE_GOODS of string
|CLR_PROVINCE_FLAG of string
|SET_PROVINCE_FLAG of string
|LIMIT of Trigger.condition list
|PROVINCE_ERROR
|RANDOM_LIST of (int * province_effect list) list
(*|TAG of string * country_effect list *)
|POP_SCOPE of pop_type * Pop_effects.pops_effect list 




let province_effects effects =
let rec province_effects_r effects out = match effects with
    |(KEYWORD,"any_pop",_)::(EQ,_,_)::(LB,_,_)::rest-> let v,rest = Pop_effects.pops_effects rest in province_effects_r           rest (POP_SCOPE(ANY_POP      ,v)::out)             
    |(KEYWORD,"aristocrats",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r        rest (POP_SCOPE(ARISTOCRATS  ,v)::out)                
    |(KEYWORD,"artisans",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r           rest (POP_SCOPE(ARTISANS     ,v)::out)             
    |(KEYWORD,"bureaucrats",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r        rest (POP_SCOPE(BUREAUCRATS  ,v)::out)                
    |(KEYWORD,"capitalists",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r        rest (POP_SCOPE(CAPITALISTS  ,v)::out)                
    |(KEYWORD,"clergymen",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r          rest (POP_SCOPE(CLERGYMEN    ,v)::out)              
    |(KEYWORD,"clerks",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r             rest (POP_SCOPE(CLERKS       ,v)::out)           
    |(KEYWORD,"craftsmen",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r          rest (POP_SCOPE(CRAFTSMEN    ,v)::out)              
    |(KEYWORD,"farmers",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r            rest (POP_SCOPE(FARMERS      ,v)::out)            
    |(KEYWORD,"labourers",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r          rest (POP_SCOPE(LABOURERS    ,v)::out)              
    |(KEYWORD,"officers",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r           rest (POP_SCOPE(OFFICERS     ,v)::out)             
    |(KEYWORD,"slaves",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r             rest (POP_SCOPE(SLAVES       ,v)::out)           
    |(KEYWORD,"soldiers",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r           rest (POP_SCOPE(SOLDIERS     ,v)::out)             
    |(KEYWORD,"poor_strata",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r        rest (POP_SCOPE(POOR_STRATA  ,v)::out)                
    |(KEYWORD,"middle_strata",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r      rest (POP_SCOPE(MIDDLE_STRATA,v)::out)                  
    |(KEYWORD,"rich_strata",_)::(EQ,_,_)::(LB,_,_)::rest->let v,rest = Pop_effects.pops_effects rest in province_effects_r        rest (POP_SCOPE(RICH_STRATA  ,v)::out)                
    |(KEYWORD,"random_list",_)::(EQ,_,_)::(LB,_,_)::rest->                        
        let rec handle_random_list tokens out = match tokens with
            |(INT,v,_)::(EQ,_,_)::(LB,_,_)::rest ->
                Printf.printf "YO %s\n" v;
                let effects,rest = province_effects_r rest [] in
                handle_random_list rest ((int_of_string v, effects)::out)
            |(RB,_,_)::rest ->
              RANDOM_LIST(out),rest 
            |rest ->
                    (*ERROR*)
                (PROVINCE_ERROR),rest
        in
        let v,rest = handle_random_list rest [] in
        province_effects_r rest ((v)::out) 

    |(KEYWORD,"limit",_)::(EQ,_,_)::(LB,_,_)::rest ->
        let pc,rest= Trigger.province_conditions rest in 
        province_effects_r rest ((LIMIT(pc))::out)
	|(KEYWORD,"assimilate",_)::(EQ,_,_)::(BOOL,v,_)::rest->
		province_effects_r rest ( ((ASSIMILATE((bool_of_string v)))::out)) 
	|(KEYWORD,"add_core",_)::(EQ,_,_)::(TAG,v,_)::rest->
		province_effects_r rest ( (ADD_CORE(v))::out) 
    |(KEYWORD,"add_core",_)::(EQ,_,_)::(SCOPE,v,_)::rest->

		province_effects_r rest ( (ADD_CORE(v))::out)
	|(KEYWORD,"add_province_modifier",_)::(EQ,_,_)::(LB,_,_)::(KEYWORD,"name",_)::(EQ,_,_)::(KEYWORD,modifier,_)::(KEYWORD,"duration",_)::(EQ,_,_)::(INT,v,_)::(LB,_,_)::rest->
		province_effects_r rest ( (ADD_PROVINCE_MODIFIER(modifier,(int_of_string v)))::out) 
	|(KEYWORD,"change_controller",_)::(EQ,_,_)::(TAG,v,_)::rest->
		province_effects_r rest ( (CHANGE_CONTROLLER(v))::out) 
	|(KEYWORD,"fort",_)::(EQ,_,_)::(INT,v,_)::rest->
		province_effects_r rest ( (FORT((int_of_string v)))::out) 
	|(KEYWORD,"infrastructure",_)::(EQ,_,_)::(INT,v,_)::rest->
		province_effects_r rest ( (INFRASTRUCTURE((int_of_string v)))::out) 
	|(KEYWORD,"life_rating",_)::(EQ,_,_)::(INT,v,_)::rest->
		province_effects_r rest ( (LIFE_RATING((int_of_string v)))::out) 
	|(KEYWORD,"naval_base",_)::(EQ,_,_)::(INT,v,_)::rest->
		province_effects_r rest ( (NAVAL_BASE((int_of_string v)))::out) 
	|(KEYWORD,"remove_core",_)::(EQ,_,_)::(TAG,v,_)::rest->
		province_effects_r rest ( (REMOVE_CORE(v))::out) 
    |(KEYWORD,"remove_core",_)::(EQ,_,_)::(SCOPE,v,_)::rest->
		province_effects_r rest ( (REMOVE_CORE(v))::out) 
	|(KEYWORD,"remove_province_modifier",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		province_effects_r rest ( (REMOVE_PROVINCE_MODIFIER(v))::out) 
	|(KEYWORD,"RGO_size",_)::(EQ,_,_)::(INT,v,_)::rest->
		province_effects_r rest ( (RGO_SIZE((int_of_string v)))::out) 
	|(KEYWORD,"secede_province",_)::(EQ,_,_)::(TAG,v,_)::rest->
		province_effects_r rest ( (SECEDE_PROVINCE(v))::out) 
    |(KEYWORD,"secede_province",_)::(EQ,_,_)::(SCOPE,v,_)::rest->
		province_effects_r rest ( (SECEDE_PROVINCE(v))::out)
	|(KEYWORD,"sub_unit",_)::(EQ,_,_)::(LB,_,_)
        ::(KEYWORD,"type",_)::(EQ,_,_)::(KEYWORD,type_value,_)
        ::(KEYWORD,"value",_)::(EQ,_,_)::(KEYWORD,v,_)
    ::(RB,_,_)::rest->
		province_effects_r rest ( (SUB_UNIT(type_value,v))::out) 
	|(KEYWORD,"trade_goods",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		province_effects_r rest ( (TRADE_GOODS(v))::out) 
	|(KEYWORD,"clr_province_flag",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		province_effects_r rest ( (CLR_PROVINCE_FLAG(v))::out) 
	|(KEYWORD,"set_province_flag",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		province_effects_r rest ( (SET_PROVINCE_FLAG(v))::out) 
    (*|(TAG,tag,_)::(EQ,_,_)::(LB,_,_)::rest->
        let effects,rest = country_effects rest  in
        province_effects_r rest (TAG(tag,effects)::out)
    *)
    |(RB,_,_)::rest ->out,rest
    |rest-> out,rest
in
province_effects_r effects []

type state_effect =
|CHANGE_REGION_NAME of string
|FLASHPOINT_TENSION of int
|IS_SLAVE of string

let state_effects effects =
let rec state_effects_r effects out = match effects with
	|(KEYWORD,"change_region_name",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		state_effects_r rest ( (CHANGE_REGION_NAME(v))::out) 
	|(KEYWORD,"flashpoint_tension",_)::(EQ,_,_)::(INT,v,_)::rest->
		state_effects_r rest ( (FLASHPOINT_TENSION((int_of_string v)))::out) 
	|(KEYWORD,"is_slave",_)::(EQ,_,_)::(BOOL,v,_)::rest->
		state_effects_r rest ( (IS_SLAVE(v))::out) 
|_->out
in
state_effects_r effects []
