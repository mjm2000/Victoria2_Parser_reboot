open TypeDef
open SymbolTable


let technology = symbol_table_init [
    (Catalog ("technology", Value Keyword), SubTable (symbol_table_init [
        Literal "area", Type "tech_area";  
        Literal "year", Year;
        Literal "cost", Value PositiveInt;
        Literal "unciv_military", Value Bool;
        Literal "max_fort", Decimal;
        Literal "unit", Decimal;
        Literal "military_tactics", Decimal;
        Literal "influence", Decimal;
        Literal "administrative_efficiency", Decimal;
        Literal "supply_range", Decimal;
        Literal "max_naval_base", Decimal;
        Literal "max_railroad", Decimal;
        Literal "max_national_focus", Decimal;
        Literal "cb_creation_speed", Decimal;
        Literal "activate_unit", Type "unit"; 
        Literal "activate_building", Type "building_def";
        Literal "permanent_prestige", Decimal;
        (Literal "ai_chance", SubTable (symbol_table_init [
            Literal "factor", Number;
            Literal "modifier",Inherit([(
                Literal "factor",  Number; 
            )], ["country_conditions_def"]);
        ]));
    ]));
]
