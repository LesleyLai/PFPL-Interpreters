open! Base
open Interpreter
open Types

let pp_to_str (pp: Caml.Format.formatter -> 'a -> unit): 'a -> string = fun a ->
  pp Caml.Format.str_formatter a; Caml.Format.flush_str_formatter ()

let pp_typ ppf = function
  | TNum -> Caml.Format.fprintf ppf "num"
  | TStr -> Caml.Format.fprintf ppf "str"

let pp_str = Caml.Format.pp_print_string
let pp_var = Caml.Format.pp_print_string
let pp_num = Caml.Format.pp_print_int

let rec pp_expr ppf: expr -> unit =
  let open Caml.Format in
  function
  | Var var -> fprintf ppf "%a" pp_var var
  | Num num -> fprintf ppf "%a" pp_num num
  | Str s -> fprintf ppf "\"%a\"" pp_str s
  | Plus(lhs, rhs) -> fprintf ppf "(@[%a@ +@ %a@])" pp_expr lhs pp_expr rhs
  | Times(lhs, rhs) -> fprintf ppf "(@[%a@ *@ %a@])" pp_expr lhs pp_expr rhs
  | Cat(lhs, rhs) -> fprintf ppf "(@[%a@ ++@ %a@])" pp_expr lhs pp_expr rhs
  | Len e -> fprintf ppf "len(%a)" pp_expr e
  | Let (e1,x,e2) -> fprintf ppf "(@[let %a@ =@ %a;@]@ %a)"
                      pp_var x pp_expr e1 pp_expr e2

let expr_to_str = pp_to_str pp_expr

let pp_typctx (gamma: TypCtx.t): string =
  "Γ = {"^
  (gamma |>
  Base.Map.fold ~init:""
    ~f:(fun ~key:k ~data:t acc ->
        acc ^ Caml.Format.sprintf "%s: %s,\n" k (pp_to_str pp_typ t)))
  ^ "}"
