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
let zeroes n: bitarray = Bytes.make n bfalse
let copy: (bitarray -> bitarray) = Bytes.copy

let uop op a = bit_of_bool (op (bool_of_bit a))
let bop op a b = bit_of_bool (op (bool_of_bit a) (bool_of_bit b))
let top op a b c = bit_of_bool (op (bool_of_bit a) (bool_of_bit b) (bool_of_bit c))
let bnot = uop Stdlib.not
let band = bop Stdlib.(&&)
let bor = bop Stdlib.(||)
let bnand = bop (fun x y -> not (x && y))
let bxor = bop (<>)
let bornot = bop (fun x y -> x || not y)
let bandnot = bop (fun x y -> x && not y)
let bmux = top (fun c x y -> if c then y else x)
let to_string (b: bitarray) =
  String.of_bytes (Bytes.map (fun b -> if b = btrue then '1' else '0') b)