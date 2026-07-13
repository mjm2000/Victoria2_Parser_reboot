let requirements = Hashtbl.create 500
let catalog_locations = Hashtbl.create 500

let type_locations = Hashtbl.create 500

let def : string option ref = ref None



let append_table symbol_table symbols = 
    let rec add_symbols symbols = match symbols with 
        |(lh_value, _)::tail when Hashtbl.mem symbol_table lh_value -> 
            add_symbols tail;
        |(lh_value, rh_value)::tail ->
            Hashtbl.add symbol_table lh_value rh_value;
            add_symbols tail
        | [] -> ()
    in
    add_symbols symbols;;


let append_requirements  dependent requirement =
    Printf.eprintf "Appending requirement: %s depends on %s\n" dependent requirement;
    (match (Hashtbl.find_opt requirements dependent) with 
    | None -> 
            Hashtbl.add requirements dependent [requirement];
    | Some existing_streams ->
        Hashtbl.replace requirements dependent (requirement :: existing_streams);
    )
let append_locations  dependent requirement locations =
    Printf.eprintf "Appending requirement: %s depends on %s\n" dependent requirement;
    (match (Hashtbl.find_opt locations dependent) with 
    | None -> 
            Hashtbl.add locations dependent [requirement];
    | Some existing_streams ->
        Hashtbl.replace locations dependent (requirement :: existing_streams);
    )

      

let file = (open_out "file.txt")
let make_order ordering =
  let tbl =
    ordering
    |> List.rev
    |> List.mapi (fun i x -> (x, i)) 
    |> List.to_seq
    |> Hashtbl.of_seq
  in
  List.iter (fun x -> Printf.fprintf file "%s\n" x) ordering;
  (fun a b ->
      match (Hashtbl.find_opt tbl a),(Hashtbl.find_opt tbl b) with
        | Some i, Some j -> compare i j
        | Some _, None -> failwith (Printf.sprintf "Element not found in ordering %s" b) 
        |None, Some _ -> failwith (Printf.sprintf "Element not found in ordering %s" a)
        |_, _ -> failwith (Printf.sprintf "Element not found in ordering %s, %s" a b)

  )


let dfs_file = (open_out "dfs_file.txt")
let depth  = ref 0



let set_requirements () = 
     Hashtbl.iter (fun def_of_type types -> 
         List.iter (fun type_label ->
            match (Hashtbl.find_opt catalog_locations type_label) with
            | Some def_of_catalog ->
                append_locations def_of_catalog def_of_type requirements; 
            |None -> failwith (Printf.sprintf "Warning: Type %s was never defined t\n" type_label)

         ) types;

     ) type_locations;
     ()



let order_requirements game_path =
    set_requirements ();
    let visited = Hashtbl.create (Hashtbl.length requirements) in
    let visiting = Hashtbl.create (Hashtbl.length requirements) in
    let result = ref [] in
    let rec visit node = 
        if (Hashtbl.mem visiting node) then 
            failwith "cycle"
        else if not (Hashtbl.mem visited node) then begin
            Hashtbl.add visiting node true;
        let cur_requirement = 
            match Hashtbl.find_opt requirements node with
            |Some xs -> xs
            |None -> []
        in
        List.iter visit cur_requirement;

        Printf.fprintf dfs_file "%s:%s\n"  (String.make (!depth * 2) ' ') node  ;
        depth := !depth + 1;
        Hashtbl.remove visiting node;
        Hashtbl.add visited node true;

        result := node :: !result
        end
    in
    Hashtbl.iter (fun dependent _ ->
        visit dependent;
    ) requirements;
    let cmp = make_order !result in
    let list = List.sort (fun (_,a) (_,b) -> cmp a b ) game_path 
    in
    list





let symbol_table_init symbols =
    lazy (
    let symbol_list_size = List.length symbols in
    (* Pre-size with 2x multiplier for better load factor (~50% load) *)
    let symbol_table = Hashtbl.create (symbol_list_size * 4) in
    let rec add_symbols symbols = match symbols with 
        |((TypeDef.Definition str) as lh_value,rh_value)::tail -> 
            Hashtbl.add symbol_table lh_value rh_value;
            def:= (Some str);
            Hashtbl.add requirements str [];
            add_symbols tail; 
        |(lh_value, ((TypeDef.Type type_lable) as rh_value))::tail -> 
            Hashtbl.add symbol_table lh_value rh_value;
            (match !def with
            | Some req ->
                append_locations req type_lable type_locations;
            | None -> 
            (*        failwith (Printf.sprintf "Warning: Type %s added without active definition requirement\n" type_lable
                      *)
            Printf.eprintf "Warning: Type %s added without active definition requirement\n" type_lable
            );
            add_symbols tail
        |(TypeDef.Catalog(catagory,symtype),rhs)::tail-> 
            Hashtbl.add symbol_table symtype (TypeDef.CatalogLeft(catagory,rhs));
            (match !def with
            | Some req ->
                (if Hashtbl.mem catalog_locations catagory then 
                    Hashtbl.add catalog_locations catagory req
                 else 
                    failwith (Printf.sprintf "Error: Catalog %s added without active definition requirement\n" catagory);
                )
            | None -> 
                Printf.eprintf "Warning: Catalog %s added without active definition requirement\n" catagory;

                    ()
            );

            add_symbols tail 
        |(lh_value,(TypeDef.Catalog(catagory,_) as rh_value))::tail-> 
            Hashtbl.add symbol_table lh_value rh_value;

            (match !def with
            | Some req ->
                append_requirements req catagory;
            | None -> ()
            );

            add_symbols tail 
        |(lh_value, rh_value)::[] ->
            Hashtbl.add symbol_table lh_value rh_value;
            add_symbols []  

        |(lh_value, rh_value)::tail ->
            Printf.eprintf "Adding symbol: %s\n" (Output.string_symbol lh_value); 
            Hashtbl.add symbol_table lh_value rh_value;
            add_symbols tail 
        | [] -> ()
    in
    add_symbols symbols; 
    symbol_table
    )

 
    

let combine_table_pair table_take table_give = 
    Hashtbl.iter (fun key value -> Hashtbl.replace (Lazy.force table_take) key value) table_give ;;

let combine_table_list tables = 
  (* Estimate size: sum of all table sizes, with 2x multiplier for load factor *)
  let estimated_size = List.fold_left (fun acc t -> acc + Hashtbl.length t) 0 tables in
  let table = lazy (Hashtbl.create (max 500 (estimated_size * 2))) in
  List.iter (fun t -> combine_table_pair table t) tables;
  table







let member symbol_table symbol = 
    Hashtbl.mem symbol_table symbol;;

(* Optimized lookup that combines membership check and retrieval *)
(* Returns Some value if found, None otherwise - avoids double lookup *)
let find_opt symbol_table symbol = 
    Hashtbl.find_opt symbol_table symbol;;

(* Fast lookup with membership check - returns (found, value_opt) *)
(* Useful when you need both membership and value *)
let find_with_mem symbol_table symbol = 
    match Hashtbl.find_opt symbol_table symbol with
    | Some v -> (true, Some v)
    | None -> (false, None);;
