let print_only = ref false
let number_steps = ref (-1)
let verbose = ref false
let rom_file = ref ""

(*let seq_simulator program inputs ?(mem_init = (fun _ size -> Array.make size false)) =
  let open Netlist_ast in
  let mangle_memory_name wsize asize name = Printf.sprintf "$mem_%s(addr_size=%d, width=%d)" name asize wsize in
  let step penv =
    let compute_arg env = function
    | Aconst c -> c
    | Avar v -> Env.find v env
    and not' = function
    | VBit b -> VBit (not b)
    | VBitArray _ -> assert false (*VBitArray (Array.map not ba)*)
    and slice i j = function
    | VBit _ -> assert false
    | VBitArray ba -> VBitArray (let res = Array.make (j-i+1) false in Array.blit ba i res 0 (j-i+1); res)
    and select i = function
    | VBit _ -> assert false
    | VBitArray ba -> VBit ba.(i)
    and binop op a b = match (a, b) with
    | VBit a, VBit b -> VBit (
      (match op with
      | And -> (&&)
      | Or -> (||)
      | Xor -> (<>)
      | Nand -> fun a b -> not (a && b)) a b
    )
    | _ -> assert false
    and concat a b = match (a, b) with
    | VBitArray a, VBitArray b -> VBitArray (Array.append a b)
    | _ -> assert false
    and mux = function
    | VBit b -> if b then fun _ b -> b else fun a _ -> a
    | VBitArray _ -> assert false
    and bit_array_to_int addr = Array.fold_right (fun b acc -> (if b then 1 else 0) + 2 * acc) addr 0
    in
    let mem env penv asize wsize name raddr we waddr wv store =
      let mem_name = mangle_memory_name asize wsize name in
      let mem = Env.find (mangle_memory_name asize wsize name) penv (* REG effect *) in
      let raddr = match raddr with
      | VBit _ -> assert false
      | VBitArray raddr ->
        assert (Array.length raddr = asize);
        bit_array_to_int raddr in
      let v = slice (wsize * raddr) ((wsize + 1) * raddr - 1) mem in
      let new_mem = match we with
      | VBit true -> (
        let waddr = match waddr with
        | VBit _ -> assert false
        | VBitArray waddr ->
          assert (Array.length waddr = asize);
          bit_array_to_int waddr in
        let mem' = match mem with
        | VBit _ -> assert false
        | VBitArray mem' -> mem' in
        let wv = match wv with
        | VBit _ -> assert false
        | VBitArray wv ->
          assert (Array.length wv = wsize);
          wv in
        let new_mem' = Array.copy mem' in
        Array.blit wv 0 new_mem' (wsize * waddr) wsize;
        VBitArray new_mem'
      )
      | _ -> mem in
      Env.update mem_name (fun _ -> Some new_mem) (store v)
    in
    let exec_stmt env (name, lhs) =
      let store v = Env.update name (fun _ -> Some v) env in
      match lhs with (* penv: env of previous iteration; env: current environment *)
    | Ereg r -> store (Env.find r penv)
    | Earg a -> store (compute_arg env a)
    | Enot a -> store (not' (compute_arg env a))
    | Eslice (i, j, a) -> store (slice i j (compute_arg env a))
    | Eselect (i, a) -> store (select i (compute_arg env a))
    | Ebinop (op, a1, a2) -> store (binop op (compute_arg env a1) (compute_arg env a2))
    | Econcat (a1, a2) -> store (concat (compute_arg env a1) (compute_arg env a2))
    | Emux (a1, a2, a3) -> store (mux (compute_arg env a1) (compute_arg env a2) (compute_arg env a3))
    | Erom (asize, wsize, raddr) -> mem env penv asize wsize name (compute_arg env raddr) (VBit false) (VBit false (* dummy *)) (VBit false (* dummy *)) store
    | Eram (asize, wsize, raddr, we, waddr, wv) -> mem env penv asize wsize name (compute_arg env raddr) (compute_arg env we) (compute_arg env waddr) (compute_arg env wv) store
    in
    List.fold_left exec_stmt penv program.p_eqs
  in
  let mems = assert false (* TODO *) in
  let init_env =
    Env.to_seq program.p_vars
    |> Seq.map (fun (name, typ) -> name,  match typ with
    | TBit -> VBit false (* TODO: revise ? *)
    | TBitArray n -> VBitArray (Array.make n false))
    |> fun s -> Seq.append s mems
    |> Env.of_seq
  in
  Seq.scan (fun env input ->
    let env = List.fold_left (fun env (iname, ival) -> Env.update iname (fun _ -> Some ival) env) env (Env.to_list input) (* TODO: probably easier to write *) in
    step env) init_env inputs*)

let simulator program number_steps =
  let load_rom =
    let build_rom _ = failwith "" in
    if !rom_file = "" then
      fun _ -> failwith "No ROM file specified"
    else
      let ic = open_in !rom_file in
      let romll = Ar.read_archive ic in
      close_in ic;
      fun name -> build_rom (List.assoc (name^"/") romll) in
  let ir = Netlist_ir.compile ~rom_init:load_rom program in
  if !verbose then Netlist_ir.print_ir_program ir;
  flush stderr;
  failwith "netlist_simulator.simulator: Non implementé"

let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate";
    "-v", Arg.Set verbose, "Verbose";
    "-r", Arg.Set_string rom_file, "AR file to load roms from"]
    compile
    ""
;;

main ()
