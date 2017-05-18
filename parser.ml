let orP f g x = f x || g x

let islower = function | 'a'..'z' -> true | _ -> false
let isupper = function | 'A'..'Z' -> true | _ -> false
let ischar = orP islower isupper
let isdigit = function | '0'..'9' -> true | _ -> false
let iswhite = function | ' ' -> true | '\t' -> true | '\n' -> true | _ -> false

exception SyntaxError of string
exception ParseError of string

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
  | OTimes
  | ODivide
  (*
  | OPlusEq
  | OMinusEq
  | OTimesEq
  | ODivideEq
  | OLT
  | OLTE
  | OGT
  | OGTE
  | OEQ
  | OAssign
  *)

type ty =
  | TInt
  | TChar
  | TBool

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

let string_of_token = function
  | Keyword KFor -> "KFor"
  | Keyword KFunc -> "KFunc"
  | Keyword KReturn -> "KReturn"
  | Keyword KConst -> "KConst"
  | Keyword KLet -> "KLet"
  | Keyword KIf -> "KIf"
  | Op _ -> "op"
  | Type _ -> "type"
  | Name n -> "name " ^ n
  | Num i -> "num " ^ string_of_int i
  | Lparen -> "(" | Rparen -> ")"
  | Lcurly -> "{" | Rcurly -> "}"
  | Colon -> ":" | Semicolon -> ";"
  | Comma -> ","

module LocationStream = struct
  type t = { stm : char Stream.t;
             mutable buf : char list;
             mutable line : int }

  let of_stream stm = ref { stm = stm; buf = []; line = 1 }
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

  let chomp1 stm =
    let _ = next stm in ()

  let push stm c =
    !stm.buf <- c :: !stm.buf

  let peek stm =
    match next stm with
    | None -> None
    | Some c -> push stm c; Some c

  let eeof _ =
    raise @@ SyntaxError "eof when reading name"

  let esyntax stm msg =
    raise @@ SyntaxError (msg ^ " on line " ^ string_of_int !stm.line)
end

module TokenStream = struct
  type t = { stm : token Stream.t; mutable buf : token list }

  let of_stream stm = ref { stm = stm; buf = [] }
  let of_list l = of_stream (Stream.of_list l)

  let next stm =
    try
      let c =
        if !stm.buf = []
        then Stream.next !stm.stm
        else ( let c = List.hd !stm.buf in !stm.buf <- List.tl !stm.buf; c )
      in
      Some c
    with | Stream.Failure -> None

  let chomp1 stm =
    let _ = next stm in ()

  let push stm c =
    !stm.buf <- c :: !stm.buf

  let peek stm =
    match next stm with
    | None -> None
    | Some c -> push stm c; Some c

  let eparse _ msg =
    raise @@ ParseError msg
end

let ctos = String.make 1
let cjoin c1 c2 = (String.make 1 c1) ^ (String.make 1 c2)

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
                 ":", Colon; ",", Comma;
                 ";", Semicolon] in
  let issym s = List.mem_assoc s symbols in
  let operators = ["+", OPlus; "-", OMinus;
                   "*", OTimes; "/", ODivide;
                   (*
                   "+=", OPlusEq; "-=", OMinusEq;
                   "*=", OTimesEq; "/=", ODivideEq;
                   "<", OLT; ">", OGT;
                   "<=", OLTE; ">=", OGTE;
                   "=", OAssign; "==", OEQ
  *)] in
  let types = ["int", TInt; "char", TChar; "bool", TBool] in
  let istype s = List.mem_assoc s types in
  let isop s = List.mem_assoc s operators in
  match next stm with
  | None -> []
  | Some c when iswhite c -> tokenize stm
  | Some c ->
      let tok =
        if issym (ctos c) then List.assoc (ctos c) symbols
        else if List.mem c ['+'; '-'; '*'; '/'; '<'; '>'; '=']
                && (peek stm) = Some '=' then
          ( chomp1 stm; Op (List.assoc (cjoin c '=') operators) )
        else if isop (ctos c) then Op (List.assoc (ctos c) operators)
        else if isdigit c then ( push stm c; Num (read_num stm 0) )
        else if ischar c then
          let w = ( push stm c; read_name stm ) in
          if iskw w then Keyword (List.assoc w keywords)
          else if istype w then Type (List.assoc w types)
          else Name w
        else esyntax stm @@ "unexpected `" ^ ctos c ^ "'"
      in tok :: (tokenize stm)

      (*
let parse stm =
  let rec p stm =
    let open TokenStream in
    let read_func stm =
      let rec read_params stm =
        match next stm with
        | Some Comma -> read_params stm
        | Some Rparen -> push stm Rparen; []
        | Some (Name n) ->
            let Some Colon = next stm in
            let Some (Type t) = next stm in
            (n, t)::(read_params stm)
        | Some t -> eparse stm @@ "Unexpected token `" ^ string_of_token t ^ "' in param list"
      in
      (* let rec read_body stm =
        match next stm with
        | Some  *)
      let Some (Name fnname) = next stm in
      let Some Lparen = next stm in
      let params = read_params stm in
      let Some Rparen = next stm in
      let Some Rcurly = next stm in
      (* let body = read_body stm in *)
      let Some Lcurly = next stm in
      Some (fnname, params) (* , body) *)
    in
    match next stm with
    | None -> []
    | Some (Keyword KFunc) ->
        (
          match read_func stm with
          | Some fn -> fn :: (p stm)
          | None -> eparse stm "Expected function"
        )
    | Some t -> eparse stm @@ "Unexpected token: `" ^ string_of_token t ^ "'"
  in
  try
    Some (p stm)
  with
  | ParseError e ->
      prerr_endline @@ "ParseError: " ^ e;
      None
  | Match_failure _ ->
      prerr_endline "ParseError: idk what";
      None
      *)

let ast_of_string f s =
  let locstream = LocationStream.of_string s in
  let tokens = tokenize locstream in
  let tokenstream = TokenStream.of_list tokens in
  f tokenstream

let read_exp stm =
  ()

let () =
  let _ = assert ("2 + 3" = "2 + 3") in
  ()
