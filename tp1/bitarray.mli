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
val zeroes : int -> bitarray
val copy : bitarray -> bitarray
val uop : (bool -> bool) -> bit -> bit
val bop : (bool -> bool -> bool) -> bit -> bit -> bit
val top : (bool -> bool -> bool -> bool) -> bit -> bit -> bit -> bit
val bnot : bit -> bit
val band : bit -> bit -> bit
val bor : bit -> bit -> bit
val bnand : bit -> bit -> bit
val bxor : bit -> bit -> bit
val bornot : bit -> bit -> bit
val bandnot : bit -> bit -> bit
val bmux : bit -> bit -> bit -> bit
