module LIR = struct
  module SymbolTable = struct
    type id = int
    type name = string
    type symbol = name * id

    let nameOf (n, i) = n

    let uid_counter = ref 0
    let gensym name =
      let uid = !uid_counter in
      ( uid_counter := uid + 1; (name, uid) )

    type 'a table = (symbol * 'a) list
    let empty = []
    exception NotFound of symbol

    let bind s v table = (s, v) :: table

    let rec find ((n, u) as s) = function
      | [] -> raise @@ NotFound s
      | ((n', u'), v')::tail when n=n' && u=u' -> v'
      | _::tail -> find s tail
  end

  module S = SymbolTable
  module A = Ast.AST

  type sym = Sym of S.symbol
  type inst =
    | Label of S.symbol
    | Move of exp * exp
    | Call of S.symbol * S.symbol * S.symbol list
    | Ret of S.symbol
    | Jump of S.symbol
    | Cjump of cmpop * S.symbol * S.symbol * S.symbol

  and exp =
      | Imm of int
      | String of string
      | Var of S.symbol
      | Addr of exp
      | Binop of binop * exp * exp

  and binop = Plus | Minus | Times | Div

  and cmpop = Eq | Neq | Lt | Le | Gt | Ge

  type program = (inst list) S.table

end
