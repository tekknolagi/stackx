type ('k, 'v) binding = 'k * 'v
type ('k, 'v) frame = ('k, 'v) binding list
type ('k, 'v) env = ('k, 'v) frame list

let empty = []

let exists n = List.exists (List.mem_assoc n)

let exists_curframe n = function
  | [] -> false
  | f::_ -> List.mem_assoc n f

let unbound n env = not @@ exists n env

let assoc n env = List.assoc n @@ List.find (List.mem_assoc n) env

let newframe env = []::env

let bind n v = function
  | [] -> [[n,v]]
  | f::fs -> ((n,v)::f)::fs

let rec zip xs ys =
  match (xs, ys) with
  | ([], []) -> []
  | (x::xs, y::ys) -> (x,y) :: zip xs ys
  | _ -> failwith "uneven"

(* Makes new frame *)
let bindlist ns vs env =
  (zip ns vs) :: env
