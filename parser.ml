module LazyStream = struct
  type 'a t = Cons of 'a * 'a t Lazy.t | Nil

  let of_stream stream =
    let rec next stream =
      try Cons(Stream.next stream, lazy (next stream))
      with Stream.Failure -> Nil
    in
    next stream

  let of_string str = str |> Stream.of_string |> of_stream
  let of_channel ic = ic |> Stream.of_channel |> of_stream
end

let orP f g x = f x || g x

let islower = function | 'a'..'z' -> true | _ -> false
let isupper = function | 'A'..'Z' -> true | _ -> false
let ischar = orP islower isupper
let isdigit = function | '0'..'9' -> true | _ -> false
let iswhite = function | ' ' -> true | '\t' -> true | '\n' -> true | _ -> false

exception SyntaxError of string

type token =
  | Name of string
  | Unexpected of char

let rec tokenize s =
  let rec read_name = function
    | LazyStream.Nil -> raise @@ SyntaxError "eof when reading name"
    | LazyStream.Cons (c, s) when iswhite c -> ""
    | LazyStream.Cons (c, s) -> String.make 1 c ^ read_name @@ Lazy.force s
  in
  match s with
  | LazyStream.Nil -> []
  | LazyStream.Cons (c, cs) as stm ->
      if iswhite c then tokenize @@ Lazy.force cs
      else
        let tok =
          if ischar c then Name (read_name stm)
          else Unexpected c
        in
        tok :: (tokenize @@ Lazy.force cs)

let _ =
  let s = " 123 abc " in
  tokenize @@ LazyStream.of_string s
