
open Lexer
type country_effect =
|SET_GLOBAL_FLAG of string
|CLR_GLOBAL_FLAG of string
|ACTIVATE_TECHNOLOGY of string
|ADD_ACCEPTED_CULTURE of string
|REMOVE_ACCEPTED_CULTURE of string
|ADD_COUNTRY_MODIFIER of string * int 
|ADD_CRISIS_INTEREST of bool
|BADBOY of int
|BUILD_FACTORY_IN_CAPITAL_STATE of string
|CAPITAL of string
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
|UPPER_HOUSE of string * string
|ADD_CASUS_BELLI of string * string * int
|ANNEX_TO of string
|CASUS_BELLI of string * string * int
|CREATE_ALLIANCE of string
|CREATE_VASSAL of string
|DIPLOMATIC_INFLUENCE of string * int 
|END_MILITARY_ACCESS of string
|END_WAR of string
|INHERIT of string
|LEAVE_ALLIANCE of string
|MILITARY_ACCESS of string
|NEUTRALITY of bool
|RELATION of string * int 
|RELEASE of string
|RELEASE_VASSAL of string
|WAR of string
|WAR_DETAILED of string * string * string * bool 
|ADD_TAX_RELATIVE_INCOME of int
|RESOURCE of string
|TREASURY of int
|CHANGE_TAG of string
|CLR_COUNTRY_FLAG of string
|SET_COUNTRY_FLAG of string
|TAG of string * country_effect list
|ANY_POP of Trigger.condition list
|ANY_OWNED of Trigger.condition list
|ALL_CORE of Trigger.condition list

|ANY_CORE of Trigger.condition list
|ANY_GREATER_POWER of Trigger.condition list
|ANY_NEIGHBOR_COUNTRY of Trigger.condition list
|ANY_OWNED_PROVINCE of Trigger.condition list
|ANY_SPHERE_MEMBER of Trigger.condition list
|ANY_STATE of Trigger.condition list
|ANY_SUBSTATE of Trigger.condition list
|CAPITAL_SCOPE of Trigger.condition list
|COUNTRY_TAG of string * Trigger.condition list
|CULTURAL_UNION of Trigger.condition list
|OVERLORD of Trigger.condition list
|REGION_NAME of string * Trigger.condition list
|SPHERE_OWNER of Trigger.condition list
|WAR_COUNTRIES of Trigger.condition list




let country_effects effects =
let rec country_effects_r effects out = match effects with
    |(KEYWORD,"any_pop",_)::(EQ,_,_)::(LB,_,_)::rest->
        let pe,rest= Trigger.pop_condition rest
        in 
        country_effects_r rest (ANY_POP(pe)::out)
    |(KEYWORD,"all_core",_)::(EQ,_,_)::(LB,_,_)::rest->
        let pe,rest= Trigger.province_conditions rest
        in 
        country_effects_r rest (ALL_CORE(pe)::out)
    |(KEYWORD,"any_core",_)::(EQ,_,_)::(LB,_,_)::rest->
        let pe,rest= Trigger.province_conditions rest
        in 
        country_effects_r rest (ALL_CORE(pe)::out)
    |(KEYWORD,"any_greater_power",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_GREATER_POWER(ce)::out)
    |(KEYWORD,"any_neighbor_country",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_NEIGHBOR_COUNTRY(ce)::out)
    |(KEYWORD,"any_owned_province",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_OWNED_PROVINCE(ce)::out)
    |(KEYWORD,"any_sphere_member",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_SPHERE_MEMBER(ce)::out)
    |(KEYWORD,"any_state",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_STATE(ce)::out)
    |(KEYWORD,"any_substate",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_SUBSTATE(ce)::out)
    |(KEYWORD,"capital_scope",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (CAPITAL_SCOPE(ce)::out)
    |(KEYWORD,"country_tag",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_STATE(ce)::out)
    |(KEYWORD,"cultural_union",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_STATE(ce)::out)

    |(KEYWORD,"overlord",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_STATE(ce)::out)

    |(KEYWORD,"region_name",_)::(EQ,_,_)::(LB,_,_)::rest->
let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_STATE(ce)::out)

    |(KEYWORD,"sphere_owner",_)::(EQ,_,_)::(LB,_,_)::rest->
        let ce,rest = Trigger.country_conditions rest in
        country_effects_r rest (ANY_STATE(ce)::out)

	|(KEYWORD,"activate_technology",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (ACTIVATE_TECHNOLOGY(v))::out) 
	|(KEYWORD,"add_accepted_culture",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (ADD_ACCEPTED_CULTURE(v))::out) 
	|(KEYWORD,"remove_accepted_culture",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (REMOVE_ACCEPTED_CULTURE(v))::out) 
	|(KEYWORD,"add_country_modifier",_)::(EQ,_,_)::(LB,_,_)::(KEYWORD,"name",_)::(EQ,_,_)::(KEYWORD,modifier,_)::(KEYWORD,"duration",_)::(EQ,_,_)::(INT,v,_)::(LB,_,_)::rest->
		country_effects_r rest ( (ADD_COUNTRY_MODIFIER(modifier,(int_of_string v)))::out) 
	|(KEYWORD,"add_crisis_interest",_)::(EQ,_,_)::(BOOL,v,_)::rest->
		country_effects_r rest ( (ADD_CRISIS_INTEREST(bool_of_string v))::out) 
	|(KEYWORD,"badboy",_)::(EQ,_,_)::(INT,v,_)::rest-> country_effects_r rest ( (BADBOY((int_of_string v)))::out) 
	|(KEYWORD,"build_factory_in_capital_state",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (BUILD_FACTORY_IN_CAPITAL_STATE(v))::out) 
	|(KEYWORD,"capital",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (CAPITAL(v))::out) 
	|(KEYWORD,"civilized",_)::(EQ,_,_)::(BOOL,v,_)::rest->
		country_effects_r rest ( (CIVILIZED(bool_of_string v))::out) 
	|(KEYWORD,"nationalvalue",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (NATIONALVALUE(v))::out) 
	|(KEYWORD,"plurality",_)::(EQ,_,_)::(INT,v,_)::rest->
		country_effects_r rest ( (PLURALITY((int_of_string v)))::out) 
	|(KEYWORD,"prestige",_)::(EQ,_,_)::(INT,v,_)::rest->
		country_effects_r rest ( (PRESTIGE((int_of_string v)))::out) 
	|(KEYWORD,"prestige_factor",_)::(EQ,_,_)::(INT,v,_)::rest->
		country_effects_r rest ( (PRESTIGE_FACTOR((int_of_string v)))::out) 
	|(KEYWORD,"primary_culture",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (PRIMARY_CULTURE(v))::out) 
    |(KEYWORD,"primary_culture",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (PRIMARY_CULTURE(v))::out)
	|(KEYWORD,"religion",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (RELIGION(v))::out) 
	|(KEYWORD,"research_points",_)::(EQ,_,_)::(INT,v,_)::rest->
		country_effects_r rest ( (RESEARCH_POINTS((int_of_string v)))::out) 
	|(KEYWORD,"war_exhaustion",_)::(EQ,_,_)::(INT,v,_)::rest->
		country_effects_r rest ( (WAR_EXHAUSTION((int_of_string v)))::out) 
	|(KEYWORD,"years_of_research",_)::(EQ,_,_)::(INT,v,_)::rest->
		country_effects_r rest ( (YEARS_OF_RESEARCH((int_of_string v)))::out) 
	|(KEYWORD,"nationalize",_)::(EQ,_,_)::(BOOL,v,_)::rest->
		country_effects_r rest ( (NATIONALIZE(bool_of_string v))::out) 
	|(KEYWORD,"economic_reform",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (ECONOMIC_REFORM(v))::out) 
	|(KEYWORD,"election",_)::(EQ,_,_)::(BOOL,v,_)::rest->
		country_effects_r rest ( (ELECTION(v))::out) 
	|(KEYWORD,"government",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (GOVERNMENT(v))::out) 
	|(KEYWORD,"military_reform",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (MILITARY_REFORM(v))::out) 
	|(KEYWORD,"political_reform",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (POLITICAL_REFORM(v))::out) 
	|(KEYWORD,"ruling_party_ideology",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (RULING_PARTY_IDEOLOGY(v))::out) 
	|(KEYWORD,"social_reform",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (SOCIAL_REFORM(v))::out) 
	|(KEYWORD,"upper_house",_)::(EQ,_,_)::(LB,_,_)::(KEYWORD,"ideology",_)::(EQ,_,_)::(KEYWORD,ideology,_)::(KEYWORD,"value",_)::(EQ,_,_)::(KEYWORD,value,_)::(LB,_,_)::rest->
		country_effects_r rest ( (UPPER_HOUSE(ideology,value))::out) 
	|(KEYWORD,"add_casus_belli",_)::(EQ,_,_)::(LB,_,_)::rest ->
         let target,rest = match rest with
            |(KEYWORD,"target",_)::(EQ,_,_)::(TAG,v,_)::rest
            |(KEYWORD,"target",_)::(EQ,_,_)::(SCOPE,v,_)::rest-> v,rest 
            |any->"",any
         in
        let casus_belli,rest = match rest with
            | (KEYWORD,"type",_)::(EQ,_,_)::(KEYWORD,v,_)::rest -> (v,rest)
            |any->"",any
        in
        let months,rest=  match rest with
            |(KEYWORD,"months",_)::(EQ,_,_)::(INT,v,_)::(RB,_,_)::rest-> (int_of_string v),rest
            |any->0,any
        in
		country_effects_r rest ((ADD_CASUS_BELLI(target,casus_belli,months)::out))
	|(KEYWORD,"annex_to",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (ANNEX_TO(v))::out) 
    |(KEYWORD,"annex_to",_)::(EQ,_,_)::(SCOPE,v,_)::rest->
		country_effects_r rest ( (ANNEX_TO(v))::out) 
	|(KEYWORD,"casus_belli",_)::(EQ,_,_)::(LB,_,_)::rest->
        let target,rest = match rest with
            |(KEYWORD,"target",_)::(EQ,_,_)::(TAG,v,_)::rest
            |(KEYWORD,"target",_)::(EQ,_,_)::(SCOPE,v,_)::rest-> v,rest 
            |any->"",any
        in
        let casus_belli,rest = match rest with
            | (KEYWORD,"type",_)::(EQ,_,_)::(KEYWORD,v,_)::rest -> (v,rest)
            |any->"",any
        in
        let months,rest=  match rest with
            |(KEYWORD,"months",_)::(EQ,_,_)::(INT,v,_)::(RB,_,_)::rest-> (int_of_string v),rest
            |any->0,any
        in
		country_effects_r rest ((CASUS_BELLI(target,casus_belli,months)::out))

	|(KEYWORD,"create_alliance",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (CREATE_ALLIANCE(v))::out) 
	|(KEYWORD,"create_vassal",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (CREATE_VASSAL(v))::out) 
	|(KEYWORD,"diplomatic_influence",_)::(EQ,_,_)::(LB,_,_)::rest->
        let target,rest = match rest with
            |(KEYWORD,"who",_)::(EQ,_,_)::(TAG,v,_)::rest
            |(KEYWORD,"who",_)::(EQ,_,_)::(SCOPE,v,_)::rest-> v,rest 
            |any->"",any
        in
        let value,rest=  match rest with
            |(KEYWORD,"months",_)::(EQ,_,_)::(INT,v,_)::(RB,_,_)::rest-> (int_of_string v),rest
            |any->0,any
        in
		country_effects_r rest (((DIPLOMATIC_INFLUENCE(target,value)))::out) 
	|(KEYWORD,"end_military_access",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (END_MILITARY_ACCESS(v))::out) 
    |(KEYWORD,"end_military_access",_)::(EQ,_,_)::(SCOPE,v,_)::rest->
		country_effects_r rest ( (END_MILITARY_ACCESS(v))::out) 
	|(KEYWORD,"end_war",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (END_WAR(v))::out) 
	|(KEYWORD,"inherit",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (INHERIT(v))::out) 
    |(KEYWORD,"inherit",_)::(EQ,_,_)::(SCOPE,v,_)::rest->
		country_effects_r rest ( (INHERIT(v))::out)
	|(KEYWORD,"leave_alliance",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (LEAVE_ALLIANCE(v))::out) 
    |(KEYWORD,"leave_alliance",_)::(EQ,_,_)::(SCOPE,v,_)::rest->
		country_effects_r rest ( (LEAVE_ALLIANCE(v))::out) 
	|(KEYWORD,"military_access",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (MILITARY_ACCESS(v))::out) 
    |(KEYWORD,"military_access",_)::(EQ,_,_)::(SCOPE,v,_)::rest->
		country_effects_r rest ( (MILITARY_ACCESS(v))::out) 
	|(KEYWORD,"neutrality",_)::(EQ,_,_)::(BOOL,v,_)::rest->
		country_effects_r rest ( (NEUTRALITY(bool_of_string v))::out) 
	|(KEYWORD,"relation",_)::(EQ,_,_)::(LB,_,_)::rest->
        let target,rest = match rest with
            |(KEYWORD,"who",_)::(EQ,_,_)::(TAG,v,_)::rest
            |(KEYWORD,"who",_)::(EQ,_,_)::(SCOPE,v,_)::rest-> v,rest 
            |any->"",any
        in
        let value,rest=  match rest with
            |(KEYWORD,"value",_)::(EQ,_,_)::(INT,v,_)::(RB,_,_)::rest-> (int_of_string v),rest
            |any->0,any
        in
		country_effects_r rest ( (RELATION(target, value))::out) 
	|(KEYWORD,"release",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (RELEASE(v))::out) 
	|(KEYWORD,"release_vassal",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (RELEASE_VASSAL(v))::out) 
    |(KEYWORD,"release_vassal",_)::(EQ,_,_)::(SCOPE,v,_)::rest->
		country_effects_r rest ( (RELEASE_VASSAL(v))::out) 
	|(KEYWORD,"war",_)::(EQ,_,_)::(TAG,v,_)::rest->
        country_effects_r rest ( (WAR(v))::out)
    |(KEYWORD,"war",_)::(EQ,_,_)::(LB,_,_)::rest->
            let target,rest = match rest with
            |(KEYWORD,"target",_)::(EQ,_,_)::(TAG,v,_)::rest -> v,rest
            |rest-> "",rest
            in
            let a_goal,rest =match rest with
            |(KEYWORD,"attacker_goal",_)::(EQ,_,_)::(LB,_,_)
            ::(KEYWORD,"casus_belli",_)::(EQ,_,_)::(KEYWORD,v,_)::(RB,_,_)::rest -> v,rest
            |_->"",rest
            in 
            let casus_belli,rest = match rest with
            |(KEYWORD,"defender_goal",_)::(EQ,_,_)::(LB,_,_)::(KEYWORD,"casus_belli",_)::(EQ,_,_)::(KEYWORD,v,_)::(RB,_,_)::rest -> v,rest
            |_->"",rest
            in
            let call_ally,rest = match rest with
            |(KEYWORD,"call_ally",_)::(EQ,_,_)::(BOOL,v,_)::(RB,_,_)::rest-> bool_of_string v, rest
            |_->false,rest
            in
            country_effects_r rest ( (WAR_DETAILED(target,a_goal,casus_belli,call_ally))::out)
		 
	|(KEYWORD,"add_tax_relative_income",_)::(EQ,_,_)::(INT,v,_)::rest->
		country_effects_r rest ( (ADD_TAX_RELATIVE_INCOME((int_of_string v)))::out) 
    (*keywords
	|(KEYWORD,name)::(EQ,_,_)::(INT,v,_)::rest->
		country_effects_r rest ( (RESOURCE((int_of_string v))::out) 
    *)
	|(KEYWORD,"treasury",_)::(EQ,_,_)::(INT,v,_)::rest->
		country_effects_r rest ( (TREASURY((int_of_string v)))::out) 
	|(KEYWORD,"change_tag",_)::(EQ,_,_)::(TAG,v,_)::rest->
		country_effects_r rest ( (CHANGE_TAG(v))::out) 
	|(KEYWORD,"clr_country_flag",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (CLR_COUNTRY_FLAG(v))::out) 
	|(KEYWORD,"set_country_flag",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (SET_COUNTRY_FLAG(v))::out) 
    |(KEYWORD,"clr_global_flag",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (CLR_GLOBAL_FLAG(v))::out) 
	|(KEYWORD,"set_global_flag",_)::(EQ,_,_)::(KEYWORD,v,_)::rest->
		country_effects_r rest ( (SET_GLOBAL_FLAG(v))::out) 

    
    |(RB,_,_)::rest->out,rest
    
    |(TAG,tag,_)::(EQ,_,_)::(LB,_,_)::rest->
        let effects,rest = country_effects_r rest [] in
        country_effects_r rest (TAG(tag,effects)::out)
    |(_,v,(x,y))::rest ->  Printf.printf "%s:%i,%i\n" v x y; out,rest
    |rest->out,rest
in
country_effects_r  effects []
