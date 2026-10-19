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

let type_size = function
| Netlist_ast.TBit -> 1
| Netlist_ast.TBitArray n -> n

let compile (p: Netlist_ast.program) ~(rom_init: Netlist_ast.ident -> bitarray) : ir_prog =
  let open Netlist_ast in
  let get_id = function
  | Avar v -> v
  | Aconst _ -> failwith "Expected ident, got constant" in
  let allocate p =
    let input_var_size, input_var_alloc = List.fold_left (fun (curr, env) x -> (* manage inputs *)
      let s = type_size (Env.find x p.p_vars) in
      curr + s, Env.add x curr env
      ) (0, Env.empty) p.p_inputs in
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
          curr_idx, Iarray.append (Env.find y bus_alloc) (Env.find z bus_alloc), ram_alloc, rom_alloc, asizes, wsizes
        | Eslice (i1, i2, a) ->
          let y = get_id a in
          let busy = Env.find y bus_alloc in
          curr_idx, Iarray.init (i2 - i1 + 1) (fun i -> busy.%(i1+1)), ram_alloc, rom_alloc, asizes, wsizes
        | Ereg _ -> curr_idx + n, Iarray.init n (fun i-> curr_idx + i), ram_alloc, rom_alloc, asizes, wsizes (* not sure if needed, but done anyway *)
        | Eram (asize, wsize, _, _, _, _) -> (* need new allocation *)
          curr_idx + n, Iarray.init n (fun i -> curr_idx + i), Env.add x (Env.cardinal ram_alloc) ram_alloc, rom_alloc, Env.add x asize asizes, Env.add x wsize wsizes
        | Erom (asize, wsize, _) -> (* need new allocation *)
          curr_idx + n, Iarray.init n (fun i -> curr_idx + i), ram_alloc, Env.add x (Env.cardinal rom_alloc) rom_alloc, Env.add x asize asizes, Env.add x wsize wsizes
        | _ -> failwith "Expected a bus but the operation is a scalar one" in
        (new_curr, Env.add x bus.%(0) var_alloc (* for dst *), Env.add x bus bus_alloc, ra, ro, asi, wsi)
      ) in
      List.fold_left alloc1 (input_var_size, input_var_alloc, Env.mapi (fun x i -> Iarray.of_array (Array.init (type_size (Env.find x p.p_vars)) (fun j -> i+j))) input_var_alloc, Env.empty, Env.empty, Env.empty, Env.empty) p.p_eqs
  in
  let var_size, var_alloc, bus_alloc, ram_alloc, rom_alloc, asizes, wsizes = allocate p in
  let roms = Array.make (Env.cardinal rom_alloc) Bitarray.epsilon in
  Env.iter (fun x rid ->
    roms.(rid) <- rom_init x;
    if Bitarray.length roms.(rid) < (1 lsl Env.find x asizes) * Env.find x wsizes
      then failwith "ROM not big enough")
  rom_alloc;
  let roms = Iarray.of_array roms in

  let find_idx x = (*Env.iter (fun x idx -> Printf.eprintf "%s -> %i\n" x idx) var_alloc; Printf.eprintf "Searching for %s\n" (match x with Avar x -> x | _ -> "");*)
    Env.find (get_id x) var_alloc
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
    [IRRom (dst, Env.find (get_id x) rom_alloc, asize, wsize, find_bus raddr)]
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

  
let print_ir_program ?(oc = Out_channel.stderr) p =
  let string_of_bus bus = String.concat "; " (Iarray.to_list (Iarray.map string_of_int bus)) in
  let string_of_instr = function
  | IRReset idx -> Printf.sprintf "v(%d) <- 0" idx
  | IRSet idx -> Printf.sprintf "v(%d) <- 0" idx
  | IRId (idx1, idx2) -> Printf.sprintf "v(%d) <- v(%d)" idx1 idx2
  | IRNot (idx1, idx2) -> Printf.sprintf "v(%d) <- ~v(%d)" idx1 idx2
  | IRReg (idx1, idx2) -> Printf.sprintf "v(%d) <- r(%d)" idx1 idx2
  | IRRegV (idx1, bus2) -> Printf.sprintf "v(%d..) <- r(%s)" idx1 (string_of_bus bus2)
  | IRAnd (idx1, idx2, idx3) -> Printf.sprintf "v(%d) <- v(%d) & v(%d)" idx1 idx2 idx3
  | IROr (idx1, idx2, idx3) -> Printf.sprintf "v(%d) <- v(%d) | v(%d)" idx1 idx2 idx3
  | IRNand (idx1, idx2, idx3) -> Printf.sprintf "v(%d) <- ~(v(%d) & v(%d))" idx1 idx2 idx3
  | IRXor (idx1, idx2, idx3) -> Printf.sprintf "v(%d) <- v(%d) ^ v(%d)" idx1 idx2 idx3
  | IRAndNot (idx1, idx2, idx3) -> Printf.sprintf "v(%d) <- v(%d) & ~v(%d)" idx1 idx2 idx3
  | IROrNot (idx1, idx2, idx3) -> Printf.sprintf "v(%d) <- v(%d) | ~v(%d)" idx1 idx2 idx3
  | IRMux (idx1, idx2, idx3, idx4) -> Printf.sprintf "v(%d) <- Mux(v(%d), v(%d), v(%d))" idx1 idx2 idx3 idx4
  | IRRom (idx, rid, asize, wsize, bus) -> Printf.sprintf "v(%d..) <- ROM(%d, asize = %d, wsize = %d)(%s)" idx rid asize wsize (string_of_bus bus)
  | IRRam (idx, rid, asize, wsize, raddr, we, waddr, wd) -> Printf.sprintf "v(%d..) <- RAM(%d, asize = %d, wsize = %d)(raddr = %s, we = v(%d), waddr = %s, wd = %s)" idx rid asize wsize (string_of_bus raddr) we (string_of_bus waddr) (string_of_bus wd)
  in
  let open Netlist_ast in
  Printf.fprintf oc "----Program IR----\n";
  Printf.fprintf oc "Total size: %d\n" p.var_size;
  Printf.fprintf oc "----Variables----\n";
  Env.iter (fun name idx -> Printf.fprintf oc "- %s: %d\n" name idx) p.var_alloc;
  Printf.fprintf oc "----Buses----\n";
  Env.iter (fun name bus -> Printf.fprintf oc "- %s: [%s]\n" name (string_of_bus bus)) p.bus_alloc;
  Printf.fprintf oc "----Ram allocations----\n";
  Env.iter (fun name idx -> Printf.fprintf oc "- %s: %d\n" name idx) p.ram_alloc;
  Printf.fprintf oc "----Rom allocations----\n";
  Env.iter (fun name idx -> Printf.fprintf oc "- %s: %d\n" name idx) p.rom_alloc;
  Printf.fprintf oc "----Address sizes----\n";
  Env.iter (fun name idx -> Printf.fprintf oc "- %s: %d\n" name idx) p.asizes;
  Printf.fprintf oc "----Word sizes----\n";
  Env.iter (fun name idx -> Printf.fprintf oc "- %s: %d\n" name idx) p.wsizes;
  Printf.fprintf oc "----Roms----\n";
  Iarray.iteri (fun rid rom -> Printf.fprintf oc "- %d (size %d): %s \n" rid (Bitarray.length rom) (Bitarray.to_string rom)) p.roms;
  Printf.fprintf oc "----Instructions----\n";
  Iarray.iteri (fun i instr -> Printf.fprintf oc "- %d: %s\n" i (string_of_instr instr)) p.instructions;
  Printf.fprintf oc "--------\n"

module IRVerfication: sig
  type verified_ir_prog = private VIRProg of ir_prog
  exception IRUnsafe
  val is_ir_program_safe: ir_prog -> bool
  val check_ir_safety: ir_prog -> verified_ir_prog
end = struct

type verified_ir_prog = VIRProg of ir_prog (* to verify its integrity for inbounds checks *)
exception IRUnsafe

let is_ir_program_safe p =
  let open Netlist_ast in
  let iok i = i >= 0 && i < p.var_size in
  let bok = Iarray.for_all iok in
  let bus_ext i b = (* extends i to a bus as wide as b *)
    Iarray.init (Iarray.length b) (fun j -> i+j)
  and bus_ext' i s = (* extends i to a bus as wide as b *)
    Iarray.init s (fun j -> i+j) in
  let rok st rid asize wsize =
    let x, _ = Env.find_first (fun x -> Env.find x st = rid) st in
    Env.find x p.asizes = asize && Env.find x p.wsizes = wsize in
  let ramok = rok p.ram_alloc and romok = rok p.rom_alloc in
  let isok = function
  | IRReset idx | IRSet idx  -> iok idx
  | IRId (idx1, idx2) | IRNot (idx1, idx2)  | IRReg (idx1, idx2) -> iok idx1 && iok idx2
  | IRRegV (idx1, bus2) -> bok (bus_ext idx1 bus2) && bok bus2
  | IRAnd (idx1, idx2, idx3) | IROr (idx1, idx2, idx3) | IRNand (idx1, idx2, idx3)
  | IRXor (idx1, idx2, idx3) | IRAndNot (idx1, idx2, idx3) | IROrNot (idx1, idx2, idx3)  -> iok idx1 && iok idx2 && iok idx3
  | IRMux (idx1, idx2, idx3, idx4) -> iok idx1 && iok idx2 && iok idx3 && iok idx4
  | IRRom (idx, rid, asize, wsize, raddr) -> bok (bus_ext' idx wsize) && romok rid asize wsize && bok raddr && Iarray.length raddr = asize
  | IRRam (idx, rid, asize, wsize, raddr, we, waddr, wd) ->
    bok (bus_ext' idx wsize) && ramok rid asize wsize &&
    bok raddr && Iarray.length raddr = asize &&
    iok we &&
    bok waddr && Iarray.length raddr = asize &&
    bok wd && Iarray.length wd = wsize
  in
  Env.for_all (Fun.const iok) p.var_alloc &&
  Env.for_all (Fun.const bok) p.bus_alloc &&
  Env.for_all (fun x rid ->
    let rom = Iarray.get p.roms rid
    and asize = Env.find x p.asizes
    and wsize = Env.find x p.wsizes in
    Bitarray.length rom >= wsize * (1 lsl asize)) p.rom_alloc &&
  Iarray.for_all isok p.instructions

let check_ir_safety p =
  if is_ir_program_safe p then
    VIRProg p
  else
    raise IRUnsafe
end

include IRVerfication

type ir_state = {
  vars: bitarray;
  rams: bitarray parray array;
  prog: ir_prog;
}

let create_ir_state (VIRProg p) =
  let rams = Array.make (Netlist_ast.Env.cardinal p.ram_alloc) (Persistent_array.Val Bitarray.epsilon) in
  Netlist_ast.Env.to_seq p.ram_alloc
  |> Seq.iter (fun (x, rid) -> rams.(rid) <- Persistent_array.make (Netlist_ast.Env.find x p.asizes) (Bitarray.zeroes (Netlist_ast.Env.find x p.wsizes)));
  {
    vars = Bitarray.zeroes p.var_size;
    rams;
    prog = p;
  }

let copy_ir_state s = {
  vars = Bitarray.copy s.vars;
  rams = Array.copy s.rams;
  prog = s.prog;
}

let update_ir_state s env =
  let open Netlist_ast in
  Env.iter (fun x v ->
    let idx = Env.find x s.prog.var_alloc in
    match v with
    | VBit b -> Bitarray.set s.vars idx (Bitarray.bit_of_bool b)
    | VBitArray ba -> Bitarray.blit (Bitarray.from_bool_array ba) 0 s.vars idx (Array.length ba)) env

let extract_ir_state (VIRProg p) s vars =
  let open Netlist_ast in
  Env.mapi (fun x ty ->
    match ty with
    | TBit -> VBit (Bitarray.bool_of_bit (Bitarray.get s.vars (Env.find x p.var_alloc)))
    | TBitArray n ->
      let bus = Env.find x p.bus_alloc in
      VBitArray (Array.init n (fun i -> Bitarray.bool_of_bit (Bitarray.get s.vars (Iarray.get bus i))))
    ) vars