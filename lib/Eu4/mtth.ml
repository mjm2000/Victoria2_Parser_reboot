open SymbolTable
open TypeDef

let country_mtth = symbol_table_init [
    (Literal("years"), Value(PositiveInt));
    (Literal("year"), Value(PositiveInt));
    (Literal("months"), Value(PositiveInt));
    (Literal("days"), Value(PositiveInt));
    (Literal("modifier"), Inherit([
        (Literal("factor"), Number);
    ], ["country_conditions_def"]));
]

let province_mtth = symbol_table_init [
    (Literal("years"), Value(PositiveInt));
    (Literal("year"), Value(PositiveInt));
    (Literal("months"), Value(PositiveInt));
    (Literal("days"), Value(PositiveInt));
    (Literal("modifier"), Inherit([
        (Literal("factor"), Number);
    ], ["province_conditions_def"]));
]
