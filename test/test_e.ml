open! Base
open! PFPL.E

let typ =
  Alcotest.testable pp_typ ( Caml.(=) )

let expr =
  Alcotest.testable pp_expr ( Caml.(=) )

let typ_checking_tests =
  let open Alcotest in
  Base.List.map ~f:
    (fun (expected, expr, text) ->
      test_case text `Quick (fun () ->
          Alcotest.(check (option typ) "same type" expected (expr_typ expr))))
    ([
      Some TNum, Num 42, "n: num";
      Some TStr, Str "x", "s: str";
      (* plus *)
      Some TNum, Plus (Num 1, Num 2), "n + n: num";
      None, Plus (Str "x", Num 2), "if e1 not num, e1 + e2 type error";
      None, Plus (Num 2, Str "x"), "if e2 not num, e1 + e2 type error";
      (* Times *)
      Some TNum, Times (Num 1, Num 2), "n * n: num";
      None, Times (Str "x", Num 2), "e1 not num => e1 * e2 type error";
      None, Times (Num 2, Str "x"), "e2 not num => e1 * e2 type error";
      (* Cat *)
      Some TStr, Cat (Str "x", Str "y"), "s1 ++ s2: str";
      None, Cat (Num 2, Str "y"), "e1 not str => e1 ++ e2 type error";
      None, Cat (Str "x", Num 2), "e2 not str => e1 ++ e2 type error";
      (* Len *)
      Some TNum, Len (Str "foo"), "e: str => len(e): num";
      None, Len (Num 42), "e not str => len(e) type error";

      (* Var *)
      None, Var "x", "Unbounded variable";

      (* Let *)
      Some TNum, Let (Num 42, "x", Var "x"), "(let x = 42; x) : num";
      Some TNum, Let (Num 42, "x", Num 1), "(let x = 42; 1) : num";
      None, Let (Num 42, "x", Var "y"), "(let x = 42; y) type error";

      (* shadowing *)
      Some TNum, Let (Str "e", "x", Let (Num 42, "x", Var "x")), "Proper handle shadowing";
    ])

let is_val_tests =
  let open Alcotest in
  Base.List.map ~f:
    (fun (expected, expr, text) ->
      test_case text `Quick (fun () ->
          Alcotest.(check bool) "same bool" expected (is_val expr)))
    [true, Num 3, "Number literals are values";
     true, Str "hello", "String literals are values";
     false, (Plus (Num 3, Num 3)), "Plus expression is not a value"]

(* Precondition: the expression e should be well typed *)
let step_tests =
  let open Alcotest in
  Base.List.map ~f:
    (fun (e, expected, text) ->
    test_case text `Quick (fun () ->
        Alcotest.(check expr) "same expr" expected (step e)))
    [Plus(Num 1, Num 2), Num 3, "1 + 2 -> 3";
     Plus(Plus(Num 1, Num 2), Plus(Num 1, Num 2)), Plus(Num 3, Plus(Num 1, Num 2)), "1 + 2 + (1 + 2) -> 3 + (1 + 2)";
     Plus(Num 3, Plus(Num 1, Num 2)), Plus(Num 3, Num 3), "3 + (1 + 2) -> 3 + 3";
     Times(Num 4, Num 2), Num 8, "4 * 2 -> 8";
     Times(Plus(Num 1, Num 2), Times(Num 1, Num 2)), Times(Num 3, Times(Num 1, Num 2)), "(1 + 2) * (1 * 2) -> 3 + (1 * 2)";
    Times(Num 3, Plus(Num 1, Num 2)), Times(Num 3, Num 3), "3 * (1 + 2) -> 3 * 3"]

let () =
  let open Alcotest in
  run "Utils" [
      "type checking", typ_checking_tests;
      "is_val", is_val_tests;
      "step", step_tests
    ]
