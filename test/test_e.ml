open! Base
open! PFPL.E

let typ =
  Alcotest.testable pp_typ ( Caml.(=) )

let typ_checking_tests =
  let open Alcotest in
  Base.List.map ~f:
    (fun (expected, expr, text) ->
      test_case text `Quick (fun () ->
          Alcotest.(check (option typ) text expected (expr_typ expr))))
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

let () =
  let open Alcotest in
  run "Utils" [
      "static-case", typ_checking_tests
    ]
