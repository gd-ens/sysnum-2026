(* Peristent arrays with efficient updating *)

type 'a parray =
| Node of 'a parray*'a parray (* LSB is 0 or 1 *)
| Val of 'a

let rec make k v =
  (* make a 2^k size one *)
  if k = 0 then
    Val v
  else
    let t' = make (k-1) v in
    Node (t', t')

let rec get a i = match (a, i ()) with
| Val v, Seq.Nil -> v
| Node (al, ah), Seq.Cons (b, i') ->
  get (if b then ah else al) i'
| _ -> invalid_arg "get"

let rec update a i f = match (a, i ()) with
| Val v, Seq.Nil -> Val (f v)
| Node (al, ah), Seq.Cons (b, i') ->
  if b then
    Node (al, update ah i' f)
  else
    Node (update al i' f, ah)
| _ -> invalid_arg "update"

let set a i v = update a i (fun _ -> v)