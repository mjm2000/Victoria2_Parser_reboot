open SymbolTable
open TypeDef
let continents = symbol_table_init [
    (Identifier, SubTable ( symbol_table_init [
        (Literal "provinces",ValueList (Catalog ("land_provid",WholeNumber)) );
        (Literal "assimilation_rate",Number);
        (Literal "farm_rgo_size",Number);
        (Literal "farm_rgo_size_factor",Number);
        (Literal "mine_rgo_size",Number);
        ])
    );

]
let regions = symbol_table_init [
    (Catalog("region",Value Keyword), ValueList(WholeNumber) ); 
]
