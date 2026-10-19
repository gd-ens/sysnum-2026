let print_only = ref false
let number_steps = ref (-1)
let verbose = ref false

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
