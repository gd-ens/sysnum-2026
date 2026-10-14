(* IR for efficient evaluation of netlists *)

(* Memory format:
  Internal state: bit array of registers, buses are selections of them
  ROMs: array of bit array, sliced to get the content
  RAMs: array of parrays of bit arrays *)

open Bitarray
open Persistent_array

(* General definitions *)

type idx = int
type rid = int (* RAM / ROM id *)
type bus = idx iarray

(* Instructions *)
type ir_instr =
(* Nullary instructions *)
| IRReset of idx (* var[idx] <- 0 *)
| IRSet of idx (* var[idx] <- 1 *)
(* Unary instructions *)
| IRId of idx*idx (* var[idx1] <- var[idx2] *)
| IRNot of idx*idx (* var[idx1] <- not var[idx2] *)
(* Registers *)
| IRReg of idx*idx (* var[idx1] <- reg[idx2] *)
| IRRegV of idx*bus (* must be allocated in a contiguous region *)
(* Binary instructions, form var[idx1] <- var[idx2] op var[idx3] *)
| IRAnd of idx*idx*idx (* && *)
| IROr of idx*idx*idx (* || *)
| IRNand of idx*idx*idx (* ~&& *)
| IRXor of idx*idx*idx (* ^ *)
(* To implement mux with a constant *)
| IRAndNot of idx*idx*idx (* a&&~b *)
| IROrNot of idx*idx*idx (* a||~b *)
(* Ternary instruction(s) *)
| IRMux of idx*idx*idx*idx (* var[idx1] <- if var[idx2] then var[idx4] else var[idx3] *)
(* Memory operation *)
| IRRom of idx*rid*int*int*bus (* rom, order specified in netlist; idx is the start of the destination bus that MUST be contiguously allocated *)
| IRRam of idx*rid*int*int*bus*idx*bus*bus (* ram, same *)

type ir_prog = {
  var_size: int;
  var_alloc: idx Netlist_ast.Env.t; (* allocation in the big memory *)
  bus_alloc: bus Netlist_ast.Env.t; (* where the bus leads to *)
  ram_alloc: rid Netlist_ast.Env.t;
  rom_alloc: rid Netlist_ast.Env.t;
  asizes: int Netlist_ast.Env.t;
  wsizes: int Netlist_ast.Env.t;
  roms: bitarray iarray;
  instructions: ir_instr iarray;
  base: Netlist_ast.program;
}

let compile (p: Netlist_ast.program) ~(rom_init: Netlist_ast.ident -> bitarray) : ir_prog =
  let open Netlist_ast in
  let get_id = function
  | Avar v -> v
  | Aconst _ -> failwith "Expected ident, got constant" in
  let allocate p =
    let alloc1 (curr_idx, var_alloc, bus_alloc, ram_alloc, rom_alloc, asizes, wsizes) (x, eq) =
      let (.%()) = Iarray.get in
      let xtyp = Env.find x p.p_vars in
      match xtyp with
      | TBit -> (let new_curr, idx = match eq with (* logic to avoid duplication with select if part of a bus *)
        | Eselect(i, a) -> let y = get_id a in curr_idx, (Env.find y bus_alloc).%(i)
          (*(match a with
          | Aconst _ -> curr_idx + 1, curr_idx(* need to allocate anyway *)
          | Avar y -> curr_idx, (Env.find y bus_alloc).(i)(* aliasing *)
        )*)
        | _ -> curr_idx + 1, curr_idx in
        (new_curr, Env.add x idx var_alloc, Env.add x (Iarray.of_array [|idx|]) bus_alloc (* to be able to treat a single bit as a bus*), ram_alloc, rom_alloc, asizes, wsizes)
      )
      | TBitArray n -> (let new_curr, bus, ra, ro, asi, wsi = match eq with
        | Econcat(a, b) ->
          let y = get_id a and z = get_id b in
          curr_idx, Iarray.append (Env.find y bus_alloc) (Env.find x bus_alloc), ram_alloc, rom_alloc, asizes, wsizes
        | Eslice (i1, i2, a) ->
          let y = get_id a in
          let busy = Env.find y bus_alloc in
          curr_idx, Iarray.init (i2 - i1 + 1) (fun i -> busy.%(i1+1)), ram_alloc, rom_alloc, asizes, wsizes
        | Ereg _ -> curr_idx + n, Iarray.init n (fun i-> curr_idx + i), ram_alloc, rom_alloc, asizes, wsizes (* not sure if needed, but done anyway *)
        | Eram (asize, wsize, _, _, _, _) -> (* need new allocation *)
          curr_idx + n, Iarray.init n (fun i-> curr_idx + i), Env.add x (snd (Env.max_binding ram_alloc) + 1) ram_alloc, rom_alloc, Env.add x asize asizes, Env.add x wsize wsizes
        | Erom (asize, wsize, _) -> (* need new allocation *)
          curr_idx + n, Iarray.init n (fun i-> curr_idx + i), ram_alloc, Env.add x (snd (Env.max_binding rom_alloc) + 1) rom_alloc, Env.add x asize asizes, Env.add x wsize wsizes
        | _ -> failwith "Expected a bus but the operation is a scalar one" in
        (new_curr, Env.add x bus.%(0) var_alloc (* for dst *), Env.add x bus bus_alloc, ra, ro, asi, wsi)
      ) in
      List.fold_left alloc1 (0, Env.empty, Env.empty, Env.empty, Env.empty, Env.empty, Env.empty) p.p_eqs
  in
  let var_size, var_alloc, bus_alloc, ram_alloc, rom_alloc, asizes, wsizes = allocate p in
  let roms = Array.make (Env.cardinal rom_alloc) Bitarray.epsilon in
  Env.iter (fun x rid ->
    roms.(rid) <- rom_init x;
    if Array.length roms.(rid) < 1 lsl (Env.find x asizes * Env.find x wsizes)
      then failwith "ROM not big enough")
  rom_alloc;
  let roms = Iarray.of_array roms in

  let find_idx x = Env.find (get_id x) var_alloc
  and find_bus x = Env.find (get_id x) bus_alloc in
  let rec compile_instruction x =
    let dst = find_idx x in function
  | Earg (Aconst (VBit true)) -> [IRSet dst]
  | Earg (Aconst (VBit false)) -> [IRReset dst]
  | Earg (Avar a as v) -> [IRId (dst, find_idx v)]
  | Earg (Aconst (VBitArray _)) -> failwith "Earg not supporting bus operation"
  | Ereg r ->
    let v = Avar r in
    (match Env.find r p.p_vars with
    | TBit -> [IRReg (dst, find_idx v)]
    | TBitArray n -> [IRRegV (dst, find_bus v)])
  | Enot (Aconst (VBit b)) -> compile_instruction x (Earg (Aconst (VBit (not b))))
  | Enot (Avar a as v) -> [IRNot (dst, find_idx v)]
  | Enot _ -> failwith "Enot not suporting bus operation"
  | Ebinop (op, a, b) -> compile_binop x op a b
  | Emux (s, a, b) -> compile_mux x s a b
  | Econcat _ | Eslice _ | Eselect _ -> [] (* nop, managed in allocation *)
  | Eram (asize, wsize, raddr, we, waddr, wd) ->
    [IRRam (dst, Env.find (get_id x) ram_alloc, asize, wsize, find_bus raddr, find_idx we, find_bus waddr, find_bus wd)]
  | Erom (asize, wsize, raddr) ->
    [IRRom (dst, Env.find (get_id x) ram_alloc, asize, wsize, find_bus raddr)]
  and compile_binop x op a b = match (op, a, b) with
  | (op, Aconst (VBit a), Aconst (VBit b)) ->
    let r = match op with
    | And -> a && b
    | Or -> a || b
    | Nand -> not (a && b)
    | Xor -> a <> b in
    compile_instruction x (Earg (Aconst (VBit r)))

  | (And, Aconst (VBit true), (Avar _ as v)) | (And,  (Avar _ as v), Aconst (VBit true)) ->
    compile_instruction x (Earg v)
  | (And, Aconst (VBit false), _) | (And, _, Aconst (VBit false)) ->
    compile_instruction x (Earg (Aconst (VBit false)))
  | (And, a, b) -> [IRAnd (find_idx x, find_idx a, find_idx b)]

  | (Or, Aconst (VBit false), (Avar _ as v)) | (Or,  (Avar _ as v), Aconst (VBit false)) ->
    compile_instruction x (Earg v)
  | (Or, Aconst (VBit true), _) | (Or, _, Aconst (VBit true)) ->
    compile_instruction x (Earg (Aconst (VBit true)))
  | (Or, a, b) -> [IROr (find_idx x, find_idx a, find_idx b)]

  | (Nand, Aconst (VBit true), (Avar _ as v)) | (Nand,  (Avar _ as v), Aconst (VBit true)) ->
    compile_instruction x (Enot v)
  | (Nand, Aconst (VBit false), _) | (Nand, _, Aconst (VBit false)) ->
    compile_instruction x (Earg (Aconst (VBit true)))
  | (Nand, a, b) -> [IRNand (find_idx x, find_idx a, find_idx b)]


  | (Xor, Aconst (VBit false), (Avar _ as v)) | (Xor,  (Avar _ as v), Aconst (VBit false)) ->
    compile_instruction x (Earg v)
  | (Xor, Aconst (VBit true), (Avar _ as v)) | (Xor,  (Avar _ as v), Aconst (VBit true)) ->
    compile_instruction x (Enot v)
  | (Xor, a, b) -> [IRXor (find_idx x, find_idx a, find_idx b)]
  and compile_mux x s a b = match s with
  | Aconst (VBit false) -> compile_instruction x (Earg a)
  | Aconst (VBit true) -> compile_instruction x (Earg b)
  | Aconst (VBitArray _) -> failwith "Mux switch must be bit, not bus"
  | Avar _ as s -> match (a, b) with
    | (Aconst (VBit b1), Aconst (VBit b2)) ->
      if b1 = b2 then
        compile_instruction x (Earg (Aconst (VBit b1)))
      else if b2 then
        compile_instruction x (Earg s) (* b1 = 0, b2 = 1 *)
      else
        compile_instruction x (Enot s)
    | (Aconst (VBit false), b) ->
      [IRAnd (find_idx x, find_idx b, find_idx s)]
    | (b, Aconst (VBit true)) ->
      [IROr (find_idx x, find_idx b, find_idx s)]
    | (Aconst (VBit true), b) ->
      [IROrNot (find_idx x, find_idx b, find_idx s)]
    | (b, Aconst (VBit false)) ->
      [IRAndNot (find_idx x, find_idx b, find_idx s)]
    | (Aconst (VBitArray _), _) | (_, Aconst (VBitArray _)) -> failwith "Vectorized mux not supported"
    | (Avar _ as a, (Avar _ as b)) ->
      [IRMux (find_idx x, find_idx s, find_idx a, find_idx b)]
  in
  let instructions =
    p.p_eqs
    |> List.map (fun (x, eq) -> compile_instruction (Avar x) eq)
    |> List.flatten
    |> Iarray.of_list in
  {
    var_size;
    var_alloc;
    bus_alloc;
    ram_alloc;
    rom_alloc;
    asizes;
    wsizes;
    roms;
    instructions;
    base = p;
  }