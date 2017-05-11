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
  type exp =
    | IntLit of int
    | CharLit of char
    | Var of name
  type statement =
    | Assignment of var * exp
    | Exp of exp
    | If of exp * statement list * statement list
  type function_def = name * var list * Type.t * statement list
  type program = function_def list
  type t = T.t * program
end

module TypedAst = AST(AnnotationCons(Type)(Location))
