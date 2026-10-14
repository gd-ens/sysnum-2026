(* IR for efficient evaluation of netlists *)

(* Memory format:
  Internal state: bit array, with single bits and buses mixed together
  ROMs: array of bit array, sliced to get the content
  RAMs: array of parrays of bit arrays *)

(* Instructions:
  LoadConstBit idx const: loads const at address idx
  CopyBit idxdst idxsrc: copies a single bit from src to dst
  NotBit idxdst idxsrc: copies the not
  RegBit idxdst idxsrc: load bit from register
  AndBit | OrBit | NandBit | XorBit idxdst idxsrc1 idxsrc2: binop with varibles
  note: binop with constants is useless as it can be replaced with one of the first three operations

  MuxBit idxdst idxchoice idxsrc1 idxsrc2: mux the sources (false -> 1, true -> 2)

  BlitBits idxdst idxsrc len: copies len bits from src to dst (for bus operations)
  BlitRegBits ...: same, but from registers
  LoadConstBits idx const: loads multiple bits at address idx
  note: concat = 2 blits, slice = 1 blit, select = 1 copy

  ReadRom idxdst romaddr idxaddr wsize asize: read wsize bits from rom romaddr into idxdst at address taken from asize bits at idxaddr
  ReadRam ...: same with ram
  WriteRam ramaddr idxaddr idxval idxwe wsize asize: write at address designated by idxaddr

  Meta information
  scratch n: tels to allocate at least n bits of scratch at idx 0 to store constants *)

open Bitarray
open Persistent_array

type idx = int

type ir_instr =
| LoadConstBit of idx * bool
| CopyBit of idx * idx
| NotBit of idx * idx
| RegBit of idx * idx
| AndBit of idx * idx * idx
| OrBit of idx * idx * idx
| NandBit of idx * idx * idx
| XorBit of idx * idx * idx
| MuxBit of idx * idx * idx * idx
| BlitBits of idx * idx * int
| BlitRegBits of idx * idx * int
| LoadConstBits of idx * bitarray
| ReadRom of idx * int * idx * int * int
| ReadRam of idx * int * idx * int * int
| WriteRam of int * idx * idx * idx * int * int

type ir_prog = {
  scratch: int; (* size of scratch *)
  allocations: int Netlist_ast.Env.t; (* allocation in the big memory *)
  roms: bitarray array;
  ram_address_sizes: int array;
  instructions: ir_instr array;
  base: Netlist_ast.program;
}

let compile (p: Netlist_ast.program) : ir_prog =

  let compile_instr (i: Netlist.equation)