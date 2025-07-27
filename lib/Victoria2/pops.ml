open SymbolTable
open TypeDef
(* 
   (Type PopTypeFile) = {
	Optional = { Left = "merge_max_size" Right = PInt }
	Optional = { Left = "max_size" Right = PInt }


	Single = { Left = "sprite" Right = PInt }
	Single = { Left = "color" Right = Color }
	Single = { Left = "strata" Right = Strata }
	Optional = { Left = "state_capital_only" Right = Bool }
	Optional = { Left = "unemployment" Right = Bool }
	Optional = { Left = "is_artisan" Right = Bool }
	Optional = { Left = "allowed_to_vote" Right = Bool }
	Optional = { Left = "is_slave" Right = Bool }
	Optional = { Left = "demote_migrant" Right = Bool }
	Optional = { Left = "can_be_recruited" Right = Bool }
	Optional = { Left = "leadership" Right = Double }
	Optional = { Left = "research_optimum" Right = Double }
	Optional = { Left = "tax_eff" Right = Double }
	Optional = { Left = "research_points" Right = Double }
	Optional = { Left = "starter_share" Right = Double }
	Optional = { Left = "workplace_output" Right = Double }
	Optional = { Left = "workplace_input" Right = Double }
	Optional = { Left = "can_work_factory" Right = Bool }
	Optional = { Left = "can_reduce_consciousness" Right = Bool }
	Optional = { Left = "factory" Right = Bool }
	Optional = { Left = "administrative_efficiency" Right = Bool }
	Optional = { Left = "can_build" Right = Bool }
	Optional = { Left = "equivalent" Right = PopType }
	
	Optional = { Left = "everyday_needs" Right = NeedsList }
	Optional = { Left = "life_needs" Right = NeedsList }
	Optional = { Left = "luxury_needs" Right = NeedsList }
	
	Optional = { Left = "everyday_needs_income" Right = NeedsIncome }
	Optional = { Left = "life_needs_income" Right = NeedsIncome }
	Optional = { Left = "luxury_needs_income" Right = NeedsIncome }
	
	Optional = { Left = "rebel" Right = RebelClause }
	
	Optional = { Left = "country_migration_target" Right = CMTClause }
	Optional = { Left = "migration_target" Right = MTClause }
	Optional = { Left = "promote_to" Right = PromotionClause }
	Optional = { Left = "ideologies" Right = IdeologyClause }
	Optional = { Left = "issues" Right = IssueClause }
}*)
(*country color*) 
let pop_file = symbol_table_init [
    Literal "merge_max_size", Value PositiveInt;
    Literal "max_size", Value PositiveInt;
    Literal "sprite", Value PositiveInt;
    Literal "color", ValueList (Value PositiveInt);
    Literal "strata", TypeOption [Literal "poor"; Literal "middle"; Literal "rich"];
    Literal "state_capital_only", Value Bool;
    Literal "unemployment", Value Bool;
    Literal "is_artisan", Value Bool;
    Literal "allowed_to_vote", Value Bool;
    Literal "is_slave", Value Bool;
    Literal "demote_migrant", Value Bool;
    Literal "can_be_recruited", Value Bool;
    Literal "leadership", Number;
    Literal "research_optimum", Number;
    Literal "tax_eff", Number;
    Literal "research_points", Number;
    Literal "starter_share", Number;
    Literal "workplace_output", Number;
    Literal "workplace_input", Number;
    Literal "can_work_factory", Value Bool;
    Literal "can_reduce_consciousness", Value Bool;
    Literal "factory", Value Bool;
    Literal "administrative_efficiency", Value Bool;
    Literal "can_build", Value Bool;
    (*Literal "equivalent", Type "pop_type";*)
    Literal "equivalent", Value Keyword;
    Literal "everyday_needs", SubTable ( symbol_table_init [
        Type "good", PositiveNumber;
    ]
    );
    Literal "life_needs", SubTable ( symbol_table_init [
        Type "good", PositiveNumber;
    ]
    );
    Literal "luxury_needs", SubTable ( symbol_table_init [
        Type "good", PositiveNumber;
    ]
    );
    Literal "everyday_needs_income", SubTable ( symbol_table_init [
        Literal "type", TypeOption [
            Literal "military";
            Literal "education"; 
            Literal "reforms";
        ]; 
        Literal "weight", PositiveNumber;
    ]
    );
    Literal "life_needs_income", SubTable ( symbol_table_init [
        Literal "type", TypeOption [
            Literal "military";
            Literal "education"; 
            Literal "reforms";
        ]; 
        Literal "weight", Number;
    ]
    );
    Literal "luxury_needs_income", SubTable ( symbol_table_init [
        Literal "type", TypeOption [
            Literal "military";
            Literal "education"; 
            Literal "reforms";
        ]; 
        Literal "weight", PositiveNumber;
    ]
    );
    Literal "rebel", SubTable ( symbol_table_init [
        Type "unit", PositiveNumber;
    ]
    );
    Literal "country_migration_target", SubTable ( symbol_table_init [
        Literal "factor", Number;
        Literal "modifier", Inherit ([
            Literal "this", Type "pop_conditions_def";
        ], ["country_conditions_def";"pop_conditions_def"]
        ) 
        ;
    ]
    );
    
]
