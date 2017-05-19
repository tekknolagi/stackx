module LIR = struct
  (* Your typical "environment" mapping names (strings) to unique identifiers.
     We are ensuring uniqueness by using a global counter that gets incremented
     for every new variable. This should help take care of scoping. *)
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
    (* Specify a label for a chunk of code. *)
    | Label of S.symbol
    (* Move the result of one expression into the location specified by the
       other. The ordering is not specified yet. *)
    | Move of exp * exp
    (* Call a label with a list of argument and some mysterious symbol
       parameter. *)
    | Call of S.symbol * S.symbol * S.symbol list
    (* Return a variable. *)
    | Ret of S.symbol
    (* Jump to a label. *)
    | Jump of S.symbol
    (* Jump to a label if the expression specified returns true. Else keep
       going. *)
    | Cjump of cmpop * S.symbol * S.symbol * S.symbol

  and exp =
    (* Immediate integral value. *)
    | Imm of int
    (* Immediate string value. *)
    | String of string
    (* Variable reference (already scoped). *)
    | Var of S.symbol
    (* Computed address. *)
    | Addr of exp
    (* Some binary expression. *)
    | Binop of binop * exp * exp

  and binop = Plus | Minus | Times | Div

  and cmpop = Eq | Neq | Lt | Le | Gt | Ge

  type program = (inst list) S.table

end
