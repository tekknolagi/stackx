module AST = struct
  module PREV = L02macros.AST
  open VM

  type t = [
    PREV.t

    | `Label of string
    | `Goto of [ op1 | `Label of string ]
    | `GotoZ of [ op1 | `Label of string ]
    | `GotoNz of [ op1 | `Label of string ]
  ]

  type without_labels = [
    PREV.t

    | `Goto of [ op1 | `Label of string ]
    | `GotoZ of [ op1 | `Label of string ]
    | `GotoNz of [ op1 | `Label of string ]
  ]

  let lower (ast : t list) =
    let find_labels instrs =
      let rec fl offset = function
        | [] -> []
        | (`Label l)::rest -> (l, offset)::(fl offset rest)
        | _::rest -> fl (offset+1) rest
      in
      fl 0 instrs
    in
    let rec without_labels = function
      | [] -> []
      | (#without_labels as x)::rst -> x::(without_labels rst)
      | _::rst -> without_labels rst
    in
    let lower_one symtab instr =
      let translate_arg = function
        | `Label l -> `Imm (List.assoc l symtab)
        | #space as x -> x
      in
      match instr with
      | `Goto x -> [`Br (translate_arg x)]
      | `GotoZ x -> [`Brz (translate_arg x)]
      | `GotoNz x -> [`Brnz (translate_arg x)]
      | #PREV.t as x -> [x]
    in
    let symtab = find_labels ast in
    List.concat @@ List.map (lower_one symtab) (without_labels ast)
end
