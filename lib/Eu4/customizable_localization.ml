open SymbolTable
open TypeDef

let customizeable_localization = 
    symbol_table_init [
        Literal "defined_text",(SubTable (symbol_table_init  [
            Literal "name", Value Keyword;
            Literal "random", Value Bool;
            Literal "text", SubTable (symbol_table_init [
                Literal "localisation_key", Catalog ( "localisation_key", Identifier);
                Literal "trigger", Type "country_conditions_def"
            ]);
        ]); 
    );
    ]
