netlist_simulator.byte: ar.ml  bitarray.ml  graph.ml  graph_test.ml  netlist_ast.ml  netlist_ir.ml  netlist.ml  netlist_printer.ml  netlist_simulator.ml  persistent_array.ml  scheduler.ml
	dune build
	ocamlbuild netlist_simulator.byte

rom:
	cd rom_test && make

clean:
	dune clean
	rm netlist_simulator.byte