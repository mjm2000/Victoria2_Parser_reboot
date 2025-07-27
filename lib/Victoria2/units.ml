open TypeDef 
open SymbolTable

(*

Single = { Left = "icon" Right = { IconRef = GFX_unit_strip } }
	Single = { Left = "sprite" Right = String } # TODO: Clarify this
	Single = { Left = "unit_type" Right = { Literal = "special" Literal = "cavalry" Literal = "transport" Literal = "light_ship" Literal = "big_ship" Literal = "infantry" (Literal If = Vic2Hod) = "support" } }
	Optional = { Left = "transport" Right = Bool }
	Optional = { Left = "active" Right = Bool }
	
	Optional = { Left = "select_sound" Right = Sound }
	Optional = { Left = "move_sound" Right = Sound }
	(Optional If = Vic2Hod) = { Left = "colonial_points" Right = Double }

	Single = { Left = "priority" Right = PInt }
	Single = { Left = "max_strength" Right = PInt }
	Single = { Left = "default_organisation" Right = PInt }
	Single = { Left = "maximum_speed" Right = Double }
	
	Single = { Left = "build_time" Right = PInt }
	Single = { Left = "build_cost" Right = { (Ch Y N) = { Left = { Type = Goods } Right = PDbl } } }
	
	Single = { Left = "supply_consumption" Right = NnDbl }
	Single = { Left = "supply_cost" Right = { (Ch Y N) = { Left = { Type = Goods } Right = PDbl } } }

 *)


(*



Switch = {
			"land" = {
				Single = { Left = "reconnaissance" Right = NnDbl }
				Single = { Left = "attack" Right = NnDbl }
				Single = { Left = "defence" Right = NnDbl }
				Optional = { Left = "siege" Right = NnDbl }
				Single = { Left = "discipline" Right = NnDbl }
				Single = { Left = "support" Right = NnDbl }
				Optional = { Left = "maneuver" Right = NnDbl }
				Optional = { Left = "primary_culture" Right = Bool } # TODO: Check if this is true. (of course not on navy as has no culture)
				(Ch Y N) = { Left = { Literal = "fort" Literal = "river" Type = Terrain } Right = {
					Optional = { Left = "attack" Right = NnDbl }
					Optional = { Left = "defence" Right = NnDbl }
					Optional = { Left = "movement" Right = NnDbl }
				} }
			}
			
			"naval" = {
				Single = { Left = "hull" Right = NnDbl }
				Single = { Left = "gun_power" Right = NnDbl }
				Optional = { Left = "capital" Right = Bool }
				
				(If If = Vic2Hod) = {
					Single = { Left = "naval_icon" Right = { IconRef = GFX_unit_strip } }
					Optional = { Left = "evasion" Right = NnDbl }
					Optional = { Left = "fire_range" Right = NnDbl }
					Optional = { Left = "torpedo_attack" Right = NnDbl }
					Optional = { Left = "supply_consumption_score" Right = NnDbl }
					Optional = { Left = "can_build_overseas" Right = Bool }

					Optional = { Left = "limit_per_port" Right = Int }
					Optional = { Left = "min_port_level" Right = Int }
				}
			}
		}








 *)
let unit_def = symbol_table_init [
    (Catalog ("unit", Value Keyword)), (SubTable (symbol_table_init [
        Literal "icon", WholeNumber;
        Literal "sprite", Value Keyword; 
        Literal "unit_type", TypeOption [
            Literal "special"; 
            Literal "cavalry"; 
            Literal "transport"; 
            Literal "light_ship"; 
            Literal "big_ship"; 
            Literal "infantry";
            Literal "support"
        ];
        Literal "transport", Value Bool;
        Literal "active", Value Bool;
        (*todo:lookup sound*)
        Literal "select_sound", Value Keyword ;
        Literal "move_sound", Value Keyword;
        Literal "colonial_points", Decimal; 
        Literal "priority", Value PositiveInt;
        Literal "max_strength", Value PositiveInt;
        Literal "default_organisation", Value PositiveInt;
        Literal "maximum_speed", Decimal;
        Literal "build_time", Value PositiveInt; 
        Literal "build_cost", SubTable (symbol_table_init [
            Type "good", Number;
        ]);
        Literal "supply_consumption", Number;
        Literal "supply_cost", SubTable (symbol_table_init [
            Type "good", Number;
        ]);
        Literal "type", Switch ([
            "land", SubTable (symbol_table_init [
                Literal "reconnaissance", Number;
                Literal "attack", Number;
                Literal "defence", Number;
                Literal "siege", Number;
                Literal "discipline", Number;
                Literal "support", Number;
                Literal "maneuver", Number;
                Literal "primary_culture", Value Bool; (* TODO: Check if this is true. (of course not on navy as has no culture) *)
                TypeOption [
                    Literal "fort"; 
                    Literal "river"; 
                    Type "terrain"
                ], SubTable (symbol_table_init [
                    Literal "attack", Number;
                    Literal "defence", Number;
                    Literal "movement", Number;
                ]);
            ]);             
            "naval", SubTable (symbol_table_init [
                Literal "hull", Number;
                Literal "gun_power", Number;
                Literal "capital", Value Bool;
                Literal "naval_icon", Integer; (* If = Vic2Hod *)
                Literal "evasion", Number;
                Literal "fire_range", Number;
                Literal "torpedo_attack", Number;
                Literal "supply_consumption_score", Number;
                Literal "can_build_overseas", Value Bool;
                Literal "limit_per_port", Integer;
                Literal "min_port_level", Integer;
            ]);
        ]);
    
]))]
