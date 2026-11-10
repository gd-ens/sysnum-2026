open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp ((_, rhs): equation) =
  List.filter_map
  (function
    | Avar v -> Some v
    | Aconst _ -> None)
  (match rhs with
    | Ereg _ -> []
    | Earg a | Enot a | Erom (_, _, a) | Eslice (_, _, a) | Eselect (_, a) -> [a]
    | Ebinop (_, a1, a2) | Econcat (a1, a2) -> [a1; a2]
    | Emux (a1, a2, a3) -> [a1; a2; a3]
    | Eram (_, _, ra, we, wa, wd) -> [ra]) (* dependency only for read; write is dealt with at the end, and should have its input already set -- hopefully *)

let schedule p =
  let g_deps = mk_graph () in
  List.iter (add_node g_deps) (List.map fst @@ Env.to_list p.p_vars);
  p.p_eqs |> List.iter (fun eq ->
    let output, _ = eq in
    let inputs = read_exp eq in
    List.iter (fun input -> add_edge g_deps input output) inputs);
  try
    let order = topological g_deps in
    let c_order = Hashtbl.create 97 in (* compiled order: ident -> int reprsenting order *)
    List.iteri (fun i v -> Hashtbl.add c_order v i) order;
    {p with
      p_eqs = List.sort (fun (v1, _) (v2, _) -> compare (Hashtbl.find c_order v1) (Hashtbl.find c_order v2)) p.p_eqs
    }
  with
  | Cycle -> raise Combinational_cycle

