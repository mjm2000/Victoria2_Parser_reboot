open TypeDef
open SymbolTable

let decisions = symbol_table_init [
    (Literal ("country_decisions"), SubTable(symbol_table_init [
        (Value (Keyword), SubTable(symbol_table_init[
            (Literal "potential", Type "country_conditions_def");
            (Literal "allow", Type "country_conditions_def");
            (Literal "effect", Type "country_effects_def");
            (Literal "ai_will_do", SubTable(symbol_table_init [
                (Literal("factor"), Number);
                (Literal ("modifier"), Inherit([
                    (Literal ("factor"), Number);
                ],["country_conditions_def"]));
            ]));
        ]));
    ]));
]
