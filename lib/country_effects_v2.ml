
open Lexer

open Pre_parser

type country_effect =
|CHANGE_VARIABLE of param list 
|SET_GLOBAL_FLAG of string
|CLR_GLOBAL_FLAG of string
|ACTIVATE_TECHNOLOGY of string
|ADD_ACCEPTED_CULTURE of string
|REMOVE_ACCEPTED_CULTURE of string
|ADD_COUNTRY_MODIFIER of param list 
|REMOVE_COUNTRY_MODIFIER of string 
|ADD_CRISIS_INTEREST of bool
|BADBOY of int
|BUILD_FACTORY_IN_CAPITAL_STATE of string
|CAPITAL of int 
|CIVILIZED of bool
|NATIONALVALUE of string
|PLURALITY of int
|PRESTIGE of int
|PRESTIGE_FACTOR of int
|PRIMARY_CULTURE of string
|RELIGION of string
|RESEARCH_POINTS of int
|WAR_EXHAUSTION of int
|YEARS_OF_RESEARCH of int
|NATIONALIZE of bool
|ECONOMIC_REFORM of string
|ELECTION of string
|GOVERNMENT of string
|MILITARY_REFORM of string |POLITICAL_REFORM of string |RULING_PARTY_IDEOLOGY of string |SOCIAL_REFORM of string
|UPPER_HOUSE of param list 
|ADD_CASUS_BELLI of param list 
|ANNEX_TO of string
|CASUS_BELLI of param list 
|CREATE_ALLIANCE of string
|CREATE_VASSAL of string
|DIPLOMATIC_INFLUENCE of param list 
|END_MILITARY_ACCESS of string
|END_WAR of string
|INHERIT of string
|LEAVE_ALLIANCE of string
|MILITARY_ACCESS of string
|NEUTRALITY of bool
|RELATION of param list 
|RELEASE of string
|RELEASE_VASSAL of string
|WAR of string
|WAR_DETAILED of param list 
|ADD_TAX_RELATIVE_INCOME of int
|RESOURCE of string
|TREASURY of int
|CHANGE_TAG of string
|CLR_COUNTRY_FLAG of string
|SET_COUNTRY_FLAG of string
|TAG of string * country_effect list
|ANY_POP of Pop_effects.pops_effect list
|ANY_OWNED of Province_effects.province_effect list
|ALL_CORE of Province_effects.province_effect list
|ANY_CORE of Province_effects.province_effect list
|ANY_GREATER_POWER of Province_effects.province_effect list
|ANY_NEIGHBOR_COUNTRY of Province_effects.province_effect list
|ANY_OWNED_PROVINCE of Province_effects.province_effect list
|ANY_SPHERE_MEMBER of Province_effects.province_effect list
|ANY_STATE of Province_effects.province_effect list
|ANY_SUBSTATE of Province_effects.province_effect list
|CAPITAL_SCOPE of Province_effects.province_effect list
|COUNTRY_TAG of string * Province_effects.province_effect list
|CULTURAL_UNION of Province_effects.province_effect list
|OVERLORD of Province_effects.province_effect list
|REGION_NAME of string * Province_effects.province_effect list
|SPHERE_OWNER of Trigger.condition list
|WAR_COUNTRIES of Province_effects.province_effect list
|LIMIT of Trigger.condition list
|RANDOM_LIST of (int * country_effect list) list
|EXCEPTION of Exception.exception_value
|ERROR



let country_effects effects =
let rec country_effects_r structures out = match structures with
    |ASSIGNMENT((KEYWORD,"limit",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Trigger.country_conditions ls in
        country_effects_r rest ((LIMIT(ce))::out)

    |ASSIGNMENT((KEYWORD,"any_pop",_),ASSIGNMENT_LIST(ls))::rest ->
         
        let pe,rest= Pop_effects.pops_effects rest
        in 
        Printf.printf "--------------\n" ;
        print_lexems rest;
        Printf.printf "--------------\n";
        country_effects_r rest ((ANY_POP(pe))::out)
    |ASSIGNMENT((KEYWORD,"all_core",_),ASSIGNMENT_LIST(ls))::rest->
        let pe,rest= Province_effects.province_effects rest
        in 
        country_effects_r rest ((ALL_CORE(pe))::out)
    |ASSIGNMENT((KEYWORD,"any_core",_),ASSIGNMENT_LIST(ls))::rest->
        let pe,rest=  Province_effects.province_effects rest
        in 
        country_effects_r rest ((ANY_CORE(pe))::out)
    |ASSIGNMENT((KEYWORD,"any_greater_power",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_GREATER_POWER(ce))::out)
    |ASSIGNMENT((KEYWORD,"owned",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_OWNED(ce))::out)
    |ASSIGNMENT((KEYWORD,"any_neighbor_country",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_NEIGHBOR_COUNTRY(ce))::out)
    |ASSIGNMENT((KEYWORD,"any_owned_province",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_OWNED_PROVINCE(ce))::out)
    |ASSIGNMENT((KEYWORD,"any_sphere_member",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_SPHERE_MEMBER(ce))::out)
    |ASSIGNMENT((KEYWORD,"any_state",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_STATE(ce))::out)
    |ASSIGNMENT((KEYWORD,"any_substate",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_SUBSTATE(ce))::out)
    |ASSIGNMENT((KEYWORD,"capital_scope",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((CAPITAL_SCOPE(ce))::out)
    |ASSIGNMENT((TAG,"country_tag",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_STATE(ce))::out)
    |ASSIGNMENT((KEYWORD,"cultural_union",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_STATE(ce))::out)

    |ASSIGNMENT((KEYWORD,"overlord",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((ANY_STATE(ce))::out)
    |ASSIGNMENT((KEYWORD,"sphere_owner",_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest ((SPHERE_OWNER(ce))::out)

	|ASSIGNMENT((KEYWORD,"activate_technology",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (ACTIVATE_TECHNOLOGY(v)))::out) 
	|ASSIGNMENT((KEYWORD,"add_accepted_culture",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (ADD_ACCEPTED_CULTURE(v)))::out) 
	|ASSIGNMENT((KEYWORD,"remove_accepted_culture",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (REMOVE_ACCEPTED_CULTURE(v)))::out) 
    |ASSIGNMENT((KEYWORD,"remove_country_modifier",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (((REMOVE_COUNTRY_MODIFIER(v)))::out) 
	|ASSIGNMENT((KEYWORD,"add_country_modifier",_),ASSIGNMENT_LIST(ls))::rest->
        let param_list = verify_params ls [(KEYWORD,"name",KEYWORD),
                              (KEYWORD,"duration",INT)] in
       country_effects_r rest (ADD_COUNTRY_MODIFIER(param_list)::out)
	|ASSIGNMENT((KEYWORD,"add_crisis_interest",_),LEXEM(BOOL,v,_))::rest->
		country_effects_r rest (((ADD_CRISIS_INTEREST(bool_of_string v)))::out) 
	|(ASSIGNMENT((KEYWORD,"badboy",_),LEXEM((INT,v,_)))::rest)-> 
        country_effects_r rest (( (BADBOY((int_of_string v))))::out) 
	|ASSIGNMENT((KEYWORD,"build_factory_in_capital_state",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (BUILD_FACTORY_IN_CAPITAL_STATE(v)))::out) 
	|ASSIGNMENT((KEYWORD,"capital",_),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (CAPITAL(int_of_string v)))::out) 
	|ASSIGNMENT((KEYWORD,"civilized",_),LEXEM(BOOL,v,_))::rest->
		country_effects_r rest (( (CIVILIZED(bool_of_string v)))::out) 
	|ASSIGNMENT((KEYWORD,"nationalvalue",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (NATIONALVALUE(v)))::out) 
	|ASSIGNMENT((KEYWORD,"plurality",_),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (PLURALITY((int_of_string v))))::out) 
	|ASSIGNMENT((KEYWORD,"prestige",_),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (PRESTIGE((int_of_string v))))::out) 
	|ASSIGNMENT((KEYWORD,"prestige_factor",_),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (PRESTIGE_FACTOR((int_of_string v))))::out) 
	|ASSIGNMENT((KEYWORD,"primary_culture",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (PRIMARY_CULTURE(v)))::out) 
    |ASSIGNMENT((KEYWORD,"primary_culture",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (PRIMARY_CULTURE(v)))::out)
	|ASSIGNMENT((KEYWORD,"religion",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (RELIGION(v)))::out) 
	|ASSIGNMENT((KEYWORD,"research_points",_),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (RESEARCH_POINTS((int_of_string v))))::out) 
	|ASSIGNMENT((KEYWORD,"war_exhaustion",_),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (WAR_EXHAUSTION((int_of_string v))))::out) 
	|ASSIGNMENT((KEYWORD,"years_of_research",_),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (YEARS_OF_RESEARCH((int_of_string v))))::out) 
	|ASSIGNMENT((KEYWORD,"nationalize",_),LEXEM(BOOL,v,_))::rest->
		country_effects_r rest (( (NATIONALIZE(bool_of_string v)))::out) 
	|ASSIGNMENT((KEYWORD,"economic_reform",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (ECONOMIC_REFORM(v)))::out) 
	|ASSIGNMENT((KEYWORD,"election",_),LEXEM(BOOL,v,_))::rest->
		country_effects_r rest (( (ELECTION(v)))::out) 
	|ASSIGNMENT((KEYWORD,"government",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (GOVERNMENT(v)))::out) 
	|ASSIGNMENT((KEYWORD,"military_reform",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (MILITARY_REFORM(v)))::out) 
	|ASSIGNMENT((KEYWORD,"political_reform",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (POLITICAL_REFORM(v)))::out) 
	|ASSIGNMENT((KEYWORD,"ruling_party_ideology",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (RULING_PARTY_IDEOLOGY(v)))::out) 
	|ASSIGNMENT((KEYWORD,"social_reform",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (SOCIAL_REFORM(v)))::out) 
	|ASSIGNMENT((KEYWORD,"upper_house",_),ASSIGNMENT_LIST(ls))::rest->
        let param_list = verify_params ls [(KEYWORD,"ideology",KEYWORD),
                              (KEYWORD,"value",KEYWORD)] in

		country_effects_r rest (( (UPPER_HOUSE(ideology,value)))::out) 
	|ASSIGNMENT((KEYWORD,"add_casus_belli",_),ASSIGNMENT_LIST(ls))::rest ->          
            let param_list = verify_params ls 
                              [(KEYWORD,"ideology",KEYWORD),
                              (KEYWORD,"value",KEYWORD)] in

		country_effects_r rest (( (UPPER_HOUSE(ideology,value)))::out)
	|ASSIGNMENT((KEYWORD,"annex_to",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (ANNEX_TO(v)))::out) 
    |ASSIGNMENT((KEYWORD,"annex_to",_),LEXEM(SCOPE,v,_))::rest->
		country_effects_r rest (( (ANNEX_TO(v)))::out) 
	|ASSIGNMENT((KEYWORD,"casus_belli",_),ASSIGNMENT_LIST(ls))::rest->
        let target,rest = match rest with
            |ASSIGNMENT((KEYWORD,"target",_),LEXEM(TAG,v,_))::rest
            |ASSIGNMENT((KEYWORD,"target",_),LEXEM(SCOPE,v,_))::rest-> v,rest 
            |any->"",any
        in
        let casus_belli,rest = match rest with
            | ASSIGNMENT((KEYWORD,"type",_),LEXEM(KEYWORD,v,_))::rest -> (v,rest)
            |any->"",any
        in
        let months,rest=  match rest with
            |ASSIGNMENT((KEYWORD,"months",_),LEXEM(INT,v,_)::(RB,_,_))::rest-> (int_of_string v),rest
            |any->0,any
        in
		country_effects_r rest (((CASUS_BELLI(target,casus_belli,months))::out))

	|ASSIGNMENT((KEYWORD,"create_alliance",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (CREATE_ALLIANCE(v)))::out) 
	|ASSIGNMENT((KEYWORD,"create_vassal",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (((CREATE_VASSAL(v)))::out) 
	|ASSIGNMENT((KEYWORD,"diplomatic_influence",_),ASSIGNMENT_LIST(ls))::rest->
        let target,rest = match rest with
            |ASSIGNMENT((KEYWORD,"who",_),LEXEM(TAG,v,_))::rest
            |ASSIGNMENT((KEYWORD,"who",_),LEXEM(SCOPE,v,_))::rest-> v,rest 
            |any->"",any
        in
        let value,rest=  match rest with
            |ASSIGNMENT((KEYWORD,"value",_),LEXEM(INT,v,_)::(RB,_,_))::rest-> (int_of_string v),rest
            |any->0,any
        in
		country_effects_r rest ((((DIPLOMATIC_INFLUENCE(target,value))))::out) 
	|ASSIGNMENT((KEYWORD,"end_military_access",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (END_MILITARY_ACCESS(v)))::out) 
    |ASSIGNMENT((KEYWORD,"end_military_access",_),LEXEM(SCOPE,v,_))::rest->
		country_effects_r rest (( (END_MILITARY_ACCESS(v)))::out) 
	|ASSIGNMENT((KEYWORD,"end_war",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (END_WAR(v)))::out) 
	|ASSIGNMENT((KEYWORD,"inherit",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (INHERIT(v)))::out) 
    |ASSIGNMENT((KEYWORD,"inherit",_),LEXEM(SCOPE,v,_))::rest->
		country_effects_r rest (( (INHERIT(v)))::out)
	|ASSIGNMENT((KEYWORD,"leave_alliance",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (LEAVE_ALLIANCE(v)))::out) 
    |ASSIGNMENT((KEYWORD,"leave_alliance",_),LEXEM(SCOPE,v,_))::rest->
		country_effects_r rest (( (LEAVE_ALLIANCE(v)))::out) 
	|ASSIGNMENT((KEYWORD,"military_access",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (MILITARY_ACCESS(v)))::out) 
    |ASSIGNMENT((KEYWORD,"military_access",_),LEXEM(SCOPE,v,_))::rest->
		country_effects_r rest (( (MILITARY_ACCESS(v)))::out) 
	|ASSIGNMENT((KEYWORD,"neutrality",_),LEXEM(BOOL,v,_))::rest->
		country_effects_r rest (( (NEUTRALITY(bool_of_string v)))::out) 
	|ASSIGNMENT((KEYWORD,"relation",_),ASSIGNMENT_LIST(ls))::rest->
        let target,rest = match rest with
            |ASSIGNMENT((KEYWORD,"who",_),LEXEM(TAG,v,_))::rest
            |ASSIGNMENT((KEYWORD,"who",_),LEXEM(SCOPE,v,_))::rest-> v,rest 
            |any->"",any
        in
        let value,rest=  match rest with
            |ASSIGNMENT((KEYWORD,"value",_),LEXEM(INT,v,_)::(RB,_,_))::rest-> (int_of_string v),rest
            |any->0,any
        in
		country_effects_r rest (( (RELATION(target, value)))::out) 
	|ASSIGNMENT((KEYWORD,"release",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (RELEASE(v)))::out) 
	|ASSIGNMENT((KEYWORD,"release_vassal",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (RELEASE_VASSAL(v)))::out) 
    |ASSIGNMENT((KEYWORD,"release_vassal",_),LEXEM(SCOPE,v,_))::rest->
		country_effects_r rest (( (RELEASE_VASSAL(v)))::out) 
	|ASSIGNMENT((KEYWORD,"war",_),LEXEM(TAG,v,_))::rest->
        country_effects_r rest (( (WAR(v)))::out)
    |ASSIGNMENT((KEYWORD,"war",_),ASSIGNMENT_LIST(ls))::rest->
            let target,rest = match rest with
            |ASSIGNMENT((KEYWORD,"target",_),LEXEM(TAG,v,_))::rest -> Some v,rest
            |rest-> None,rest
            in
            let a_goal,rest =match rest with
            |ASSIGNMENT((KEYWORD,"attacker_goal",_)::(EQ,_,_))::ASSIGNMENT_LIST(ls)
            ::ASSIGNMENT((KEYWORD,"casus_belli",_),LEXEM(KEYWORD,v,_)::(RB,_,_))::rest -> Some v,rest
            |_->None,rest
            in 
            let casus_belli,rest = match rest with
            |ASSIGNMENT((KEYWORD,"defender_goal",_),ASSIGNMENT_LIST(ls))::
                ASSIGNMENT((KEYWORD,"casus_belli",_),LEXEM(KEYWORD,v,_))::rest ->Some v,rest
            |_->None,rest
            in
            let call_ally,rest = match rest with
            |ASSIGNMENT((KEYWORD,"call_ally",_),LEXEM(BOOL,v,_)::(RB,_,_)::(RB,_,_))::rest-> (Some (bool_of_string v)), rest
            |ASSIGNMENT((RB,_,_)::(RB,_,_))::rest ->   None,rest
            |_-> None,rest
            in


            country_effects_r rest (( (WAR_DETAILED(target,a_goal,casus_belli,call_ally)))::out)
		 
	|ASSIGNMENT((KEYWORD,"add_tax_relative_income",_),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (ADD_TAX_RELATIVE_INCOME((int_of_string v))))::out) 
    (*keywords
	|ASSIGNMENT((KEYWORD,name),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (RESOURCE((int_of_string v)))::out) 
    *)

	|ASSIGNMENT((KEYWORD,"treasury",_),LEXEM(INT,v,_))::rest->
		country_effects_r rest (( (TREASURY((int_of_string v))))::out) 
	|ASSIGNMENT((KEYWORD,"change_tag",_),LEXEM(TAG,v,_))::rest->
		country_effects_r rest (( (CHANGE_TAG(v)))::out) 
    |ASSIGNMENT((KEYWORD,"change_tag_no_core_switch",_),LEXEM(TAG,v,_))::rest->
            (*change this later*)
		country_effects_r rest (( (CHANGE_TAG(v)))::out)
    |ASSIGNMENT((KEYWORD,"change_variable",_),ASSIGNMENT_LIST(ls))::rest->
            let v,rest= match rest  with
            |ASSIGNMENT((KEYWORD,"which",_),LEXEM(KEYWORD,v,_))::
            ASSIGNMENT((KEYWORD,"value",_),LEXEM(INT,i,_)::(RB,_,_))::rest-> (CHANGE_VARIABLE(v,LEXEM(int_of_string i))),rest
            |rest->ERROR,rest
        in
		country_effects_r rest ((v)::out)


	|ASSIGNMENT((KEYWORD,"clr_country_flag",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (CLR_COUNTRY_FLAG(v)))::out) 
	|ASSIGNMENT((KEYWORD,"set_country_flag",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (SET_COUNTRY_FLAG(v)))::out) 
    |ASSIGNMENT((KEYWORD,"clr_global_flag",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (CLR_GLOBAL_FLAG(v)))::out) 
	|ASSIGNMENT((KEYWORD,"set_global_flag",_),LEXEM(KEYWORD,v,_))::rest->
		country_effects_r rest (( (SET_GLOBAL_FLAG(v)))::out) 

    |ASSIGNMENT((KEYWORD,"random_list",_),ASSIGNMENT_LIST(ls))::rest->
        let rec handle_random_list tokens out = match tokens with
            |ASSIGNMENT((INT,v,_),ASSIGNMENT_LIST(ls))::rest ->
                let effects,rest = country_effects_r rest [] in
                handle_random_list rest (((int_of_string v, effects))::out)
            |ASSIGNMENT((RB,_,_))::rest ->
              RANDOM_LIST(out),rest 
            |_::rest ->
                    (*ERROR*)
                (ERROR),rest
            |[]->
                (ERROR),[]
        in
        let v,rest = handle_random_list rest [] in
        country_effects_r rest (((v))::out) 

    |ASSIGNMENT((KEYWORD,key,_),ASSIGNMENT_LIST(ls))::rest->
        let ce,rest = Province_effects.province_effects rest in
        country_effects_r rest ((REGION_NAME(key,ce))::out)
    
    |ASSIGNMENT((TAG,tag,_),ASSIGNMENT_LIST(ls))::rest->
        let effects,rest = country_effects_r rest [] in
        country_effects_r rest ((TAG(tag,effects))::out)
    |ASSIGNMENT((KEYWORD,v,(x,y)),LEXEM(lex_type,v2,_))::rest -> 
            let exp = EXCEPTION(Exception.UNEXPECTED_ASSIGNMENT(v,KEYWORD,v2,lex_type),(x,y)) in
            (List.rev (exp::out) ),rest
    |[] -> (List.rev out),rest
    |rest-> (List.rev out),rest
in
country_effects_r  effects []
