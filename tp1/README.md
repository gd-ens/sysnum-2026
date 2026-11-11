# Netlist simulator
This is an implementation by Gabriel Deleage of the netlist simulator for the "Systèmes Numériques" lecture, session 2026/2027.

This is personal work and shall not be used without explicit permission of the author.
Permission is granted to ENS's "Systèmes Numériques" teaching staff to use this for evaluation purposes.

## Building
- `make` to build netlist_simulator.byte; then you can execute it
- `make clean` to remove dune build files
- `make distclean` to also remove the generated executable
- `make testrom` to build the test ROM in `rom_test` (see more on ROM in a dedicated section)

## Usage
`./netlist_simulator.byte [OPTIONS] FILE` where `FILE` is in the netlist format
### Options
-  `-n` Number of steps to simulate
-  `-v` Verbose (debug informations on stdout)
-  `-p` Persistence required during evaluation (each step evaluated is not overridden but copied; more robust than faster option, useful to check in case of a bug)
-  `--lsb` Inputs and outputs lsb first (defaults to msb first)
-  `--rom` file to load roms from, in ar format (see ROMS)
-  `--no-prompt` Remove prompts (for integration)
-  `-help` or `--help`  Display help
### Interacting
The program asks for input with prompts of the form
`<variable_name> [: bus_width] =`.
No bus width indicate a single bit value. Input must be in binary MSB (unless overridden by the `--lsb` option).

Outputs are precceded with prompts
`<variable_name> =`.

One can override prompts with the `--no-prompt` options.
The order remains the same as with prompts, so one can use it to help the program interact with another program.
### ROMs
If the program uses any ROMS, there must be loaded with `--rom`.
They must be stored in an `ar` archive file, in packed (8 bits per byte, LSB first) format.
A ROM is accepted if it is at least `wsize * (2 ** asize)` in size (where `asize` is the address size and `wsize` the word size).
They stored as a big bit array, a word being a `wsize` aligned sequence of `wsize` bits, and any extra bits are discarded.

## Inner workings
The simulator uses an internal virtual machine and intermediate representation.

### Virtual machine
It is comprised of:
- the current states of all variables, in a linear bitarray (denoted `var`);
- the previous states of all variables, for registers (denoted `reg`);
- a set of ROMs, which are big bit arrays, identified by a ROM id (`rid`);
- a set of RAMs, which are persistent arrays, to allow for efficent updates while keeping persistence and for lazy memory allocation, identified by a RAM id (`rid` also, but dealt with separately).

Each variable is assigned an index to be reffered to in the bitarrays `var` and `reg`, sort of a memory location.

Buses (except for input and RAMs/ROMs) have no physical existence (to avoid useless copies), and are just a list of indices. They are fully handled at compile time.

### IR
It uses a simple IR, that improves on simple instruction by:
- limiting depth to 1 (fast pattern matching);
- stores the precomuted indices instead of variable names.

It mimicks closely the orginal instructions (except for RAM, split in read and write).
See `netlist_ir.ml` for details and `netlist_simulator.ml` for exact semantics.