module FunctionalHashTable : sig
  type ('k, 'v) t
  val empty : ('k, 'v) t
  val add : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t
  val find : ('k, 'v) t -> 'k -> 'v option
  val remove : ('k, 'v) t -> 'k -> ('k, 'v) t
  end = struct
  module M = Map.Make(String) (* Change String to any comparable type *)

  type ('k, 'v) t = 'v M.t

  let empty = M.empty

  let add table key value = M.add key value table

  let find table key = 
    try Some (M.find key table) 
    with Not_found -> None

  let remove table key = M.remove key table
end







let symbol_table_init symbols =
    let symbol_list_size = List.length symbols in
    let symbol_table = Hashtbl.create symbol_list_size in
    let rec add_symbols symbols = match symbols with 
        (*|(lh_value, rh_value)::tail when Hashtbl.mem symbol_table lh_value -> 
            raise Invalid_Argument ("Symbol " ^ lh_value ^ " already exists") *)
        |(lh_value, rh_value)::tail ->
            Hashtbl.add symbol_table lh_value rh_value;
            add_symbols tail
        | [] -> ()
    in
    add_symbols symbols; 
    symbol_table

let append_table symbol_table symbols = 
    let rec add_symbols symbols = match symbols with 
        |(lh_value, _)::_ when Hashtbl.mem symbol_table lh_value -> 
                ()
        |(lh_value, rh_value)::tail ->
            Hashtbl.add symbol_table lh_value rh_value;
            add_symbols tail
        | [] -> ()
    in
    add_symbols symbols; 
    symbol_table
