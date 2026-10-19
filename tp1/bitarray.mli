type bitarray
type bit
val bfalse : bit
val btrue : bit
val bit_of_bool : bool -> bit
val bool_of_bit : bit -> bool
val from_bool_array : bool array -> bitarray
val get : bitarray -> int -> bit
val set : bitarray -> int -> bit -> unit
val blit : bitarray -> int -> bitarray -> int -> int -> unit
val epsilon : bitarray
val length : bitarray -> int