open Symbol_table
open Type_def

let bookmarks = symbol_table_init [
    KEYWORD_SYMBOL "bookmark",PARAM_LIST (symbol_table_init [
        KEYWORD_SYMBOL "name", PARAM_VALUE STRING;
        KEYWORD_SYMBOL "desc", PARAM_VALUE STRING; 
        KEYWORD_SYMBOL "date", PARAM_VALUE KEYWORD;
        KEYWORD_SYMBOL "cameraX", PARAM_VALUE INT;
        KEYWORD_SYMBOL "cameraY", PARAM_VALUE INT;
        ]
    ); 
]


(*
aeroplane_factory = {
	type = factory
	on_completion = factory
	completion_size = 0.2
	max_level = 99
	goods_cost =
	{
		machine_parts = 200
		electric_gear = 600
		steel = 600
		cement = 600
	}
	time = 730
	visibility = yes
	onmap = no

	production_type = aeroplane_factory
	pop_build_factory = yes
	advanced_factory = yes
}
*)
let buildings = symbol_table_init [
    (TYPE_SYMBOL KEYWORD, PARAM_LIST (symbol_table_init [
        (KEYWORD_SYMBOL "type", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "on_completion", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "completion_size", PARAM_VALUE FLOAT);
        (KEYWORD_SYMBOL "max_level", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "goods_cost", PARAM_LIST (symbol_table_init [
            (TYPE_SYMBOL KEYWORD, PARAM_VALUE INT);
        ]
        ));
        (KEYWORD_SYMBOL "time", PARAM_VALUE INT);
        (KEYWORD_SYMBOL "visibility", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "onmap", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "production_type", PARAM_VALUE KEYWORD);
        (KEYWORD_SYMBOL "pop_build_factory", PARAM_VALUE BOOL);
        (KEYWORD_SYMBOL "advanced_factory", PARAM_VALUE BOOL);
    ]));
]
let cb_types = symbol_table_init [
    (KEYWORD_SYMBOL "peace_order", VALUE_LIST (PARAM_VALUE KEYWORD));


]
