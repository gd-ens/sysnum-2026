open Netlist_ir
let print_only = ref false
let number_steps = ref (-1)
let verbose = ref false

let simulate_ir_step (VIRProg p) (renv: ir_state) (cenv: ir_state): unit =
  let exec_instr = Bitarray.(function
  | IRReset idx -> set cenv.vars idx bfalse
  | IRSet idx -> set cenv.vars idx btrue
  | IRId (idx1, idx2) -> set cenv.vars idx1 (get cenv.vars idx2)
  | IRNot (idx1, idx2) -> set cenv.vars idx1 (bnot (get cenv.vars idx2))
  | IRReg (idx1, idx2) -> set cenv.vars idx1 (get renv.vars idx2)
  | IRRegV (idx1, bus2) -> Iarray.iteri (fun offset idx2 -> set cenv.vars (idx1+offset) (get cenv.vars idx2)) bus2
  | IRAnd (idx1, idx2, idx3) -> set cenv.vars idx1 (band (get cenv.vars idx2) (get cenv.vars idx3))
  | IROr (idx1, idx2, idx3) -> set cenv.vars idx1 (bor (get cenv.vars idx2) (get cenv.vars idx3))
  | IRNand (idx1, idx2, idx3) -> set cenv.vars idx1 (bnand (get cenv.vars idx2) (get cenv.vars idx3))
  | IRXor (idx1, idx2, idx3) -> set cenv.vars idx1 (bxor (get cenv.vars idx2) (get cenv.vars idx3))
  | IRAndNot (idx1, idx2, idx3) -> set cenv.vars idx1 (bandnot (get cenv.vars idx2) (get cenv.vars idx3))
  | IROrNot (idx1, idx2, idx3) -> set cenv.vars idx1 (bornot (get cenv.vars idx2) (get cenv.vars idx3))
  | IRMux (idx1, idx2, idx3, idx4) -> set cenv.vars idx1 (bmux (get cenv.vars idx2) (get cenv.vars idx3) (get cenv.vars idx4))
  | IRRom (idx, rid, asize, wsize, raddr) -> blit (Iarray.get p.roms rid) (0 * wsize) cenv.vars idx wsize
  | IRRam (idx, rid, asize, wsize, raddr, we, waddr, wd) ->
    let get_bus bus = Iarray.map (Fun.compose bool_of_bit (get cenv.vars)) bus in
    let raddr = get_bus raddr (* get the bits associated with the bus *) in
    blit (Persistent_array.get cenv.rams.(rid) (Iarray.to_seq raddr)) 0 cenv.vars idx wsize;
    if bool_of_bit (get cenv.vars we) then
      let waddr = get_bus waddr
      and wd = from_bool_array (Iarray.to_array (get_bus wd)) in
      cenv.rams.(rid) <- Persistent_array.set cenv.rams.(rid) (Iarray.to_seq waddr) wd
  ) in
  Iarray.iter exec_instr p.instructions


let simulator program number_steps =
  let ir = Netlist_ir.compile ~rom_init:(fun _ -> Bitarray.epsilon) program in
  if !verbose then Netlist_ir.print_ir_program ir;
  let vir = Netlist_ir.check_ir_safety ir in
  if !verbose then Printf.eprintf "Safty checks passed\n";
  flush stderr;
  Printf.eprintf "Not implemented\n"; ignore vir;
  ()

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
    "-v", Arg.Set verbose, "Verbose"]
    compile
    ""
;;

main ()
