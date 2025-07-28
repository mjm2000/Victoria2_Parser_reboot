open TypeDef

let lexem_type_string lexem = match lexem with
| NegativeFloat ->    "NegativeFloat" 
| NegativeInt  ->     "NegativeInt"
| PositiveFloat ->    "PositiveFloat" 
| PositiveInt  ->     "PositiveInt"
| Keyword ->  "Keyword"
| Tag  ->     "Tag" 
| String  ->  "String"
| Scope ->    "Scope"
| Condition ->"Conditional"
| Bool ->     "Bool"      
| Date ->     "Date"
let print_memory_stats label =
  let stat = Gc.stat () in
  Printf.printf "[%s] Heap size: %.2f MB | Live words: %d | Free words: %d | Major collections: %d\n%!"
    label
    ((float_of_int stat.Gc.heap_words *. float_of_int (Sys.word_size / 8)) /. 1024.0 /. 1024.0)
    stat.Gc.live_words
    stat.Gc.free_words
    stat.Gc.major_collections
;;


let lexem_to_str lexem =match lexem with 
| LB  ->      "Left Brace"
| RB  ->      "Right Brace"
| EQ ->       "Equal"
| LexemValue l -> Printf.sprintf "Literal %s" (lexem_type_string l)
| LexError ->"Lex Error"

let string_lexem lex_value = 
  let (lex, str, (x, y)) = lex_value in
  Printf.sprintf "%s %s (%i,%i)" (lexem_to_str lex) str x y


let string_lexems lexems = 
  List.fold_left (fun buffer lexem -> Printf.sprintf "%s\n%s" buffer (string_lexem lexem)) "" lexems

let print_lexem lexem = 
  Printf.printf "%s: " (lexem_to_str lexem)

let print_lexems lexems = 
  List.iter (fun (lex, str, (x, y)) ->
    print_lexem lex;
    Printf.printf "%s (%i,%i)\n" str x y
  ) lexems

let rec exception_iden_string exp = match exp with
    | TYPE_MISHMASH (iden, expected, received) ->
        Printf.sprintf "Incorrect Type: Text=%s Expected Type=%s Received Type=%s" 
            iden (lexem_type_string expected) (lexem_type_string received)  
    
    | END_OF_FILE -> "Unexpected EOF"  
    | UNKNOWN_IDENTIFIER string -> Printf.sprintf "Unknown Identifier: %s" string  
    | UNEXPECTED_LEXEM (iden, lexem) -> 
        Printf.sprintf "Unexpected Token: Text=%s Type=%s" iden (lexem_to_str lexem) 

    | UNEXPECTED_ASSIGNMENT (assignment) ->
        Printf.sprintf "Unexpected Assignment: %s" (string_assignment assignment)

    | UNEXPECTED_ASSIGN_LIST (e) ->
        Printf.sprintf "Unexpected Assignments:%s" (string_assignment_list e) 

    | UNEXPECTED_RIGHT_BRACKET -> "Unexpected Right Bracket"
    | UNEXPECTED_LEFT_BRACKET -> "Unexpected Left Bracket"
    | MULTIPLE_CHOICE(ls)-> 
            Printf.sprintf "Multiple Choice %s" (exception_lists_string (ls))
    | UNEXPECTED_EXPR_LIST (ls) ->
        List.fold_left (fun acc x -> Printf.sprintf "%s\n\t%s" acc (string_expr x)) "" ls
    | UNEXPECTED_EXPR (e) -> string_expr e
    | UNEXPECTED_EQUAL -> "Unexpected Equal"

and exception_string (exp : exception_value) : string = 
    (*add expected value printer*)
    let ev, e, (x, y),file = exp in 
    Printf.sprintf "%s:(%i,%i):\n\t%s, expected:%s" file x y (exception_iden_string e) (string_expected_value ev)
and exceptions_string ls = 
    let rec exceptions__r ls out = match ls with
        | [] -> out
        | x :: rest -> 
            let x = exception_string x in
            exceptions__r rest (Printf.sprintf "%s\n%s" out x)
    in
    exceptions__r ls ""
and exception_lists_string ls = 
    let rec exception_lists__r ls out = match ls with
        | [] -> out
        | x :: rest -> 
            let x = exceptions_string x in
            exception_lists__r rest (Printf.sprintf "%s\n\t%s" out x)
    in
    exception_lists__r ls ""

and string_assignment assignment = match assignment with  
    | ASSIGNMENT ((tlh, vlh, (x,y)), rh) ->
        let expr = string_expr rh in
        Printf.sprintf "ASSIGNMENT(type:%s,location:(%d,%d),lhvalue:%s=rhvalue:%s)" (lexem_to_str  tlh) x y vlh expr 

    | EXPR expr ->
        Printf.sprintf "EXPR(rhvalue:%s)" (string_expr expr) 
    | ASSIGN_EXCEPTION exception_value -> exception_string exception_value

and string_assignment_list al =
    let x = List.fold_left 
        (fun acc x -> Printf.sprintf "%s\n\t%s" acc (string_assignment x)) 
        "" al 
    in 
    Printf.sprintf "ASSIGN_LIST(%s)" x

and string_expr expr = match expr with
    | LEXEM (type_val, str, (x, y)) -> 
        Printf.sprintf "LEXEM(type(%s):value(%s),location(%d,%d))" (lexem_to_str type_val) str x y 
    | LEXEM_LIST lexem_list ->
        let x = List.fold_left (fun acc x -> 
            Printf.sprintf "%s\n\t%s" acc (string_expr x)
        ) "" lexem_list in
        Printf.sprintf "LEXEM_LIST(%s)" x
    | ASSIGNMENT_LIST assignment_list -> string_assignment_list assignment_list
    | EXPR_EXCEPTION exception_value -> exception_string exception_value

and  string_value_type value_type = match value_type with
|Keyword -> "Keyword"
|Bool  -> "Bool" 
|PositiveInt-> "PositiveInt" 
|NegativeInt-> "NegativeInt" 
|NegativeFloat -> "NegativeFloat" 
|PositiveFloat-> "PositiveFloat" 
|String-> "String"
|Tag  -> "Tag"
|Scope-> "Scope"
|Condition-> "Condition"
|Date-> "Date"

and string_symbol pv = match pv with
    | Year -> "Year"
    | SubTable (symbol_table) -> 
        let x = Hashtbl.fold (fun k v acc -> 
            Printf.sprintf "%s\n\t%s:%s" acc (string_symbol k) (string_symbol v)
        ) symbol_table "" in
        Printf.sprintf "SubTable(%s)" x
    | TypeOption (options) ->
        let x = List.fold_left (fun acc x -> 
            Printf.sprintf "%s\n\t%s" acc (string_symbol x)
        ) "" options in
        Printf.sprintf "TypeOption(%s)" x
    | Value str -> Printf.sprintf "LexemValue:%s" (string_value_type str)
    | ValueList (value) ->
        Printf.sprintf "Value List(%s)" (string_symbol value) 
    |Switch ls ->
        let x = List.fold_left (fun acc (key,value) -> 
            Printf.sprintf "%s\n\t%s:%s" acc  key (string_symbol value)
        ) "" ls in
        Printf.sprintf "Switch(%s)" x
    | WholeNumber -> "NUMBER"
    |Link  -> "LINK"
    |Inherit (symbols, table_names) ->
        let x = List.fold_left (fun acc (lh, rh) -> 
            Printf.sprintf "%s\n\t%s:%s" acc (string_symbol lh) (string_symbol rh)
        ) "" symbols in
        let y = List.fold_left (fun acc x -> 
            Printf.sprintf "%s\n\t%s" acc x
        ) "" table_names in
        Printf.sprintf "{Symbols(%s):Inherits(%s)}"  x y;
    |Integer -> "Integer"
    |Decimal -> "Decimal"
    |Type t -> Printf.sprintf "TypeRight:%s" t
    |SubType (t,v) -> Printf.sprintf "SubType:%s,%s" v t
    |SupType t -> Printf.sprintf "SubType:%s" t
    |InnerCatalog (v,t) ->  Printf.sprintf "Inner Catalog:%s:%s" v (string_symbol t)
    |Catalog (v, t) -> Printf.sprintf "Catalog:%s:%s" v (string_symbol t)
    |SubCatalog(supv,subv, t) -> Printf.sprintf "SubCatalog:(%s,%s,%s)" supv subv (string_symbol t)
    |SupCatalog (v, t) -> Printf.sprintf "Sup_Catalog:%s:%s" v (string_symbol t)

    |Definition (v) -> Printf.sprintf "Definition:%s" v
    |SubDefinition (v,t) -> Printf.sprintf "SubDefinition:(Sup:%s,Sub:%s)" v t
    |Number -> "Number"
    |PositiveNumber -> "PositiveNumber"
    |NegativeNumber -> "NegativeNumber"
    |Literal (v) -> Printf.sprintf "Literal:%s" v
    |Identifier -> Printf.sprintf "Identifier" 
    |Target -> "Target"
    |Dir v -> Printf.sprintf "Dir:%s" v
    |CatalogFile (place,regex,rhv) -> Printf.sprintf "CatalogFile:(catagory:%s,regex:%s,rhs:%s)" place regex  (string_symbol rhv)
    |Nothing -> "Nothing"




and string_symbol_table symbol_table =
    let x = Hashtbl.fold (fun k v acc -> 
        Printf.sprintf "%s\n\t%s:%s" acc (string_symbol k) (string_symbol v)
    ) symbol_table "" in
    Printf.sprintf "SYMBOL_TABLE(%s)" x

and string_expected_value v = match v with
    | RHS(ls) -> 
        let x = List.fold_left (fun acc x -> 
            match x with
            | SubTable(_)-> Printf.sprintf "%s\n\t%s" acc "SubTable"
            |_ -> Printf.sprintf "%s\n\t%s" acc (string_symbol x)
        ) "" ls in
        Printf.sprintf "RHS(%s)" x 
    | LHS(ls) -> 
        let x = List.fold_left (fun acc x -> 
            Printf.sprintf "%s\n\t%s" acc (string_symbol x)
        ) "" ls in
        Printf.sprintf "LHS(%s)" x
    |ExpEqual -> "ExpEqual"
    |ExpLeftBracket -> "ExpLeftBracket"
    |ExpRightBracket -> "ExpRightBracket"
    | NONE -> "NONE"

let get_errors assignments = 
    let rec get_errors_r assignments out = match assignments with
    | ((ASSIGN_EXCEPTION (e)) :: rest) ->
        get_errors_r rest (e :: out)
    | ASSIGNMENT(_,EXPR_EXCEPTION e) :: rest-> 
        get_errors_r rest (e :: out)
    | ASSIGNMENT(_,ASSIGNMENT_LIST al) :: rest ->
        let assign_list__errors = get_errors_r al out in
        get_errors_r rest assign_list__errors
    | [] -> out
    | _::rest -> get_errors_r rest out
in
get_errors_r assignments []
