open SymbolTable
open TypeDef

let areas = symbol_table_init [
    (Catalog("area", Value Keyword), ValueList (Catalog ("land_provid", WholeNumber)));
]

let regions = symbol_table_init [
    (Catalog("region", Value Keyword), SubTable (symbol_table_init [
        (Literal "areas", SubTable (symbol_table_init [
            (Value Keyword, ValueList (Value Keyword));
        ]));
    ]));
]

let superregions = symbol_table_init [
    (Catalog("superregion", Value Keyword), ValueList (Type "region"));
]

let continents = symbol_table_init [
    (Catalog("continent", Value Keyword), ValueList (Catalog ("land_provid", WholeNumber)));
]

let provincegroups = symbol_table_init [
    (Catalog("provincegroup", Value Keyword), ValueList (Type ("land_provid")));
]
