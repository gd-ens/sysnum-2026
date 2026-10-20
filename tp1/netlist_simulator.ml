open Netlist_ir
let print_only = ref false
let number_steps = ref (-1)
let verbose = ref false
let persistent = ref false
let lsb_first = ref false
let rom_file = ref ""
let no_prompt = ref false

let simulate_ir_step (VIRProg p) (renv: ir_state) (cenv: ir_state): unit =
  let get_bus bus = Bitarray.(Iarray.map (Fun.compose bool_of_bit (get cenv.vars)) bus)
  and bits_to_int ba = Iarray.fold_right (fun b acc -> 2*acc + if b then 1 else 0) ba 0 in
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
  | IRRom (idx, rid, asize, wsize, raddr) -> blit (Iarray.get p.roms rid) (bits_to_int (get_bus raddr) * wsize) cenv.vars idx wsize
  | IRRam (idx, rid, asize, wsize, raddr, we, waddr, wd) ->
    let raddr = get_bus raddr (* get the bits associated with the bus *) in
    blit (Persistent_array.get cenv.rams.(rid) (Iarray.to_seq raddr)) 0 cenv.vars idx wsize;
    if bool_of_bit (get cenv.vars we) then
      let waddr = get_bus waddr
      and wd = from_bool_array (Iarray.to_array (get_bus wd)) in
      cenv.rams.(rid) <- Persistent_array.set cenv.rams.(rid) (Iarray.to_seq waddr) wd
  ) in
  Iarray.iter exec_instr p.instructions

let persistent_simulator p inputs =
  Seq.scan (fun renv input ->
    let cenv = copy_ir_state renv in
    update_ir_state cenv input;
    simulate_ir_step p renv cenv;
    cenv) (create_ir_state p) inputs

module NonPersistent: sig
  val non_persistent_simulator: verified_ir_prog -> (Netlist_ast.value Netlist_ast.Env.t -> ir_state) end = struct

type non_persistent = {
  mutable cenv: ir_state;
  mutable renv: ir_state;
}

let non_persistent_simulator p =
  let renv = create_ir_state p in
  let cenv = copy_ir_state renv in
  let internal = {renv; cenv;} in
  fun input ->
    let {renv; cenv;} = internal in
    let cenv, renv = renv, cenv in (* swap the use of the two, renv now no longer being useful *)
    Array.blit renv.rams 0 cenv.rams 0 (Array.length renv.rams); (* necessary, otherwise we do not get the up to date content; does not matter on other registers *)
    update_ir_state cenv input;
    simulate_ir_step p renv cenv;
    internal.renv <- renv; internal.cenv <- cenv;
    cenv
end
include NonPersistent

let get_inputs ?(lsb_first = false) (VIRProg p) =
  let open Netlist_ast in
  let get_input x =
    let t = Env.find x p.base.p_vars in
    let prompt, length = match t with
    | TBit -> x ^ " = ", 1
    | TBitArray n -> x ^ ": " ^ string_of_int n ^ " = ", n in
    let prompt = if !no_prompt then "" else prompt in
    let rec loop () =
      print_string prompt;
      let l = String.trim (read_line ()) in
      if String.length l <> length then begin
        Printf.eprintf "Incorrect length\n";
        flush stderr;
        loop ()
      end
      else begin
        try
          let bool_of_char = function
          | '0' -> false | '1' -> true | _ -> invalid_arg "bool_of_char" in
          match t with
          | TBit -> VBit (bool_of_char l.[0])
          | TBitArray _ -> VBitArray (Array.init length (fun i -> bool_of_char l.[if lsb_first then i else length - 1 - i]))
        with Invalid_argument _ ->
          Printf.eprintf "Invalid format\n";
          flush stderr;
          loop ()
      end in
    loop () in
  List.fold_left (fun env x -> Env.add x (get_input x) env) Env.empty p.base.p_inputs

let get_outputs (VIRProg p as vp) s =
  let open Netlist_ast in
  let output_env = List.fold_left (fun env x -> Env.add x (Env.find x p.base.p_vars) env) Env.empty p.base.p_outputs in
  extract_ir_state vp s output_env

let print_outputs ?(lsb_first = false) p s =
  Netlist_ast.Env.iter (fun x v ->
    Printf.printf (if !no_prompt then "%s%s\n" else "%s = %s\n") (if !no_prompt then "" else x) (
      let char_of_bool b = if b then '1' else '0' in
      match v with
      | Netlist_ast.VBit b -> String.make 1 (char_of_bool b)
      | Netlist_ast.VBitArray ba ->
        Array.to_seq ba
        |> Seq.map char_of_bool
        |> (if lsb_first then Fun.id else (fun seq -> List.to_seq @@ List.rev @@ List.of_seq seq))
        |> String.of_seq
  )) (get_outputs p s)

let simulator program number_steps =
  let load_rom =
    let build_rom s =
      s 
      |> String.to_seq
      |> Seq.map (fun c -> let i = int_of_char c in Array.map (fun shift -> i lsr shift mod 2 = 1) [|0;1;2;3;4;5;6;7|])
      |> List.of_seq
      |> Array.concat
      |> Bitarray.from_bool_array in
    if !rom_file = "" then
      fun _ -> failwith "No ROM file specified"
    else
      let ic = open_in !rom_file in
      let romll = Ar.read_archive ic in
      close_in ic;
      fun name ->
        let s = List.assoc (name^"/") romll in
        if !verbose then (
          Printf.eprintf "Building ROM from string \"%s\"\n" (String.escaped s);
          flush stderr
        );
        build_rom s in
  let ir = Netlist_ir.compile ~rom_init:load_rom program in
  if !verbose then Netlist_ir.print_ir_program ir;
  let vir = Netlist_ir.check_ir_safety ir in
  if !verbose then Printf.eprintf "Safety checks passed\n";
  flush stderr;
  let inputs = Seq.of_dispenser (fun _ -> try Some (get_inputs ~lsb_first:!lsb_first vir) with End_of_file -> print_newline (); None) in
  let states =
    if !persistent then
      persistent_simulator vir inputs
    else
      let nps = non_persistent_simulator vir in
      Seq.map nps inputs in
  let states =
    if number_steps >= 0 then
      Seq.take number_steps states
    else
      states in
  if not !no_prompt then Printf.printf "----Step 0----\n";
  Seq.iteri (fun i state -> print_outputs ~lsb_first:!lsb_first vir state; if not !no_prompt then Printf.printf "--------\n----Step %d----\n" (i+1);) states

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
    "-p", Arg.Set persistent, "Persistence required during evaluation";
    "--lsb", Arg.Set lsb_first, "Inputs and outputs lsb first";
    "--rom", Arg.Set_string rom_file, "AR file to load roms from";
    "--no-prompt", Arg.Set no_prompt, "Remove prompts (for integration)"]
    compile
    ""
;;

main ()
