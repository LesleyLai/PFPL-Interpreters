open! Base
open Types

let unimp: string -> 'a = fun s -> failwith ("Unimplemented: " ^ s)

module TypCtx = struct
  type t = typ Map.M(String).t

  let empty: t = Map.empty (module String)
  let lookup: var -> t -> typ option =
    fun x gamma -> Map.find gamma x
  let extend: var -> typ -> t -> t = fun x tau gamma ->
    gamma |> Map.set ~key:x ~data:tau
end

(* Static of Expr following chapter 4 *)
let rec expr_typ: TypCtx.t -> expr -> typ option = fun gamma ->
  let open Base.Option in
  function
  | Num _ -> Some TNum
  | Str _ -> Some TStr
  | Plus (e1, e2) | Times (e1, e2) -> expr_typ_binop gamma e1 e2 TNum TNum TNum
  | Cat (e1, e2) -> expr_typ_binop gamma e1 e2 TStr TStr TStr
  | Len e -> expr_typ_uop gamma e TStr TNum
  | Var x -> gamma |> TypCtx.lookup x
  | Let (e1, x, e2) ->
     expr_typ gamma e1 >>= fun tau1 ->
     let gamma' = gamma |> TypCtx.extend x tau1 in
     expr_typ gamma' e2

and expr_typ_uop gamma e expect rtype =
  let open Base.Option in
  let open Base.Poly in
  expr_typ gamma e >>= fun tau ->
  Option.some_if (tau = expect) rtype

and expr_typ_binop gamma lhs rhs lhs_expect rhs_expect rtype =
  let open Base.Option in
  let open Base.Poly in
  expr_typ gamma lhs >>= fun tau1 ->
  expr_typ gamma rhs >>= fun tau2 ->
  Option.some_if (tau1 = lhs_expect && tau2 = rhs_expect) rtype

let expr_typ = TypCtx.empty |> expr_typ
