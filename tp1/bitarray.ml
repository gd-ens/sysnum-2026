type bitarray = bool array
let from_bool_array: (bool array -> bitarray) = Array.copy
let set = Array.set
let blit = Array.blit
let epsilon = [||]
let length = Array.length