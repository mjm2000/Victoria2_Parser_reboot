open TypeDef
open SymbolTable
let decisions = symbol_table_init [
    (Literal ("political_decisions"),SubTable(symbol_table_init [
        (Value (Keyword),SubTable(symbol_table_init[
            (Literal "potential",Type "country_conditions_def");
            (Literal "allow",Type "country_conditions_def");
            (Literal "effect",Type "country_effects_def");
            (Literal "news",Value(Bool));
            (Literal "news_title",Value(String));
            (Literal "news_desc_short",Value(String));
            (Literal "news_desc_medium",Value(String));
            (Literal "news_desc_long",Value(String));
            (Literal "news_desc_image",Value(String));
            (Literal ("picture"), TypeOption([
                Value(String);
                Value(Keyword)
            ]));
            (Literal "alert",Value(Bool));
            (Literal ("ai_will_do"),SubTable(symbol_table_init [
                (Literal("factor"),
                    Number);
                (Literal ("modifier"),Inherit([
                    (Literal ("factor"),Number);
            ],["country_conditions_def"]));
            ]));
            
        ]));

    ])
    );
] 
