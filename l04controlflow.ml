module AST = struct
  module PREV = L03labels.AST
  open VM

  type t = [
    PREV.t

    | `If of (space * t list * t list)
    | `Ifz of (t list * t list)
    | `While of (space * t list)
  ]

  let labelcount = ref 0
  let rec lower (ast : t list) =
    let inclabel () =
      let c = !labelcount in
      ( labelcount := !labelcount + 1; c)
    in
    let nextlabel () =
      let c = inclabel () in
      `Label ("__if_lbl__" ^ string_of_int c)
    in
    let lower_one = function
      | `If (cond, iftrue, iffalse) ->
          [`Test cond] @ lower [`Ifz (iftrue, iffalse)]
      | `Ifz (ifz, ifnz) ->
          let ifzlbl = nextlabel () in
          let endlbl = nextlabel () in
          [`GotoZ ifzlbl]
          @ lower ifnz
          @ [ifzlbl]
          @ lower ifz
          @ [endlbl]
      | `While (cond, body) ->
          let bodylbl = nextlabel () in
          let endlbl = nextlabel () in
          lower [bodylbl;
                 `If (cond, body @ [`Goto bodylbl], [`Goto endlbl]);
                 endlbl]
      | #PREV.t as x -> [x]
    in
    List.concat @@ List.map lower_one ast
end
