type bitarray = bytes
type bit = char
let bfalse: bit = '\000'
let btrue: bit = '\001'
let bit_of_bool b = if b then btrue else bfalse
let bool_of_bit b = b = btrue
let from_bool_array ba: bitarray = Bytes.init (Array.length ba) (fun i -> bit_of_bool ba.(i))
let get: (bitarray -> int -> bit) = Bytes.get
let set: (bitarray -> int -> bit -> unit) = Bytes.set
let blit: (bitarray -> int -> bitarray -> int -> int -> unit) = Bytes.blit
let epsilon: bitarray = Bytes.init 0 (fun _ -> '\000')
let length: (bitarray -> int) = Bytes.length