module Type = struct
  type t = Int | String
end

module AnnotationCons (A : sig type t end) (B : sig type t end) = struct
  type t = A.t * B.t
end

module Location = struct
  type t = int * int
end

module AST (T : sig type t end) = struct
  type name = string
  type var = name * Type.t
  type op =
    | Plus
    | Minus
    | Times
    | Div
  type exp =
    | IntLit of int
    | CharLit of char
    | Var of name
    | InfixOper of op * exp * exp
    | Funcall of name * exp list
  type statement =
    | Assignment of var * exp
    | If of exp * statement list * statement list
    | Return of exp
    | Exp of exp
  type function_def = Fun of name * var list * Type.t * statement list
  type program = Prog of function_def list
  type t = T.t * program
end

module TypedAst = AST(AnnotationCons(Type)(Location))

let () =
  let open TypedAst in
  let prog = Prog
    [
      Fun ("main", [], Type.Int, [
        Return (IntLit 0)
      ])
    ] in
    ignore prog
