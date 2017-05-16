let orP f g x = f x || g x

let islower = function | 'a'..'z' -> true | _ -> false
let isupper = function | 'A'..'Z' -> true | _ -> false
let ischar = orP islower isupper
let isdigit = function | '0'..'9' -> true | _ -> false
let iswhite = function | ' ' -> true | '\t' -> true | '\n' -> true | _ -> false

exception SyntaxError of string

type keyword =
  | KFor
  | KFunc
  | KReturn
  | KConst
  | KLet
  | KIf

type op =
  | OPlus
  | OMinus

type ty =
  | TInt
  | TChar

type token =
  | Keyword of keyword
  | Op of op
  | Type of ty
  | Name of string
  | Num of int
  | Lparen
  | Rparen
  | Lcurly
  | Rcurly
  | Colon
  | Comma
  | Semicolon
  | Eq

module LocationStream = struct
  type t = { stm : char Stream.t;
             mutable buf : char list;
             mutable line : int }

  let of_stream stm = { stm = stm; buf = []; line = 1 }
  let of_string s = of_stream @@ Stream.of_string s

  let next stm =
    try
      let c =
        if !stm.buf = []
        then Stream.next !stm.stm
        else ( let c = List.hd !stm.buf in !stm.buf <- List.tl !stm.buf; c )
      in
      let () = if c = '\n' then !stm.line <- !stm.line + 1 in
      Some c
    with | Stream.Failure -> None

  let push stm c =
    !stm.buf <- c :: !stm.buf

  let eeof _ =
    raise @@ SyntaxError "eof when reading name"

  let esyntax stm msg =
    raise @@ SyntaxError (msg ^ " on line " ^ string_of_int !stm.line)
end

let ctos = String.make 1

let rec tokenize stm =
  let open LocationStream in
  let rec read_name stm =
    match next stm with
    | None -> eeof stm
    | Some c when not @@ ischar c -> push stm c; ""
    | Some c-> ctos c ^ read_name stm
  in
  let rec read_num stm acc =
    match next stm with
    | None -> eeof stm
    | Some c when not @@ isdigit c -> push stm c; acc
    | Some c -> read_num stm @@ ((Char.code c) - (Char.code '0')) + 10*acc
  in
  let keywords = ["for", KFor; "func", KFunc; "return", KReturn; "let", KLet;
                  "const", KConst; "if", KIf] in
  let iskw w = List.mem_assoc w keywords in
  let symbols = ["(", Lparen; ")", Rparen;
                 "{", Lcurly; "}", Rcurly;
                 ":", Colon; ",", Comma; ";", Semicolon; "=", Eq] in
  let issym s = List.mem_assoc s symbols in
  let operators = ["+", OPlus; "-", OMinus] in
  let types = ["int", TInt; "char", TChar] in
  let istype s = List.mem_assoc s types in
  let isop s = List.mem_assoc s operators in
  match next stm with
  | None -> []
  | Some c when iswhite c -> tokenize stm
  | Some c ->
      let tok =
        if issym (ctos c) then List.assoc (ctos c) symbols
        else if isop (ctos c) then Op (List.assoc (ctos c) operators)
        else if isdigit c then ( push stm c; Num (read_num stm 0) )
        else if ischar c then
          let w = ( push stm c; read_name stm ) in
          if iskw w then Keyword (List.assoc w keywords)
          else if istype w then Type (List.assoc w types)
          else Name w
        else esyntax stm @@ "unexpected `" ^ ctos c ^ "'"
      in tok :: (tokenize stm)

let _ =
  let s = "
  func add (a : int, b : int) {
    let sum : int = a + b;
    return sum;
  }

  func main () : int {
    const a : int = 5;
    return add(3, a);
  }
  " in
  let () = print_endline s in
  let stm = LocationStream.of_string s in
  let toks = tokenize @@ ref stm in
  toks
