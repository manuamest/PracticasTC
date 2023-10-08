#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

(* Función para verificar si una gramática está en Forma Normal de Chomsky *)
let es_fnc = function
  | Gic (_, _, Conjunto reglas, _) ->
    List.for_all
      (fun (Regla_gic (lhs, rhs)) ->
        match lhs, rhs with
        | No_terminal _, [Terminal _] -> true
        | No_terminal _, [No_terminal _; No_terminal _] -> true
        | _ -> false)
      reglas;;

(* Gramática en FNC *)
let g1 = gic_of_string "S A B; a b c; S; S -> A B; A -> a; B -> b;";;
es_fnc g1;; (* Devuelve true *)

(* Gramática no en FNC *)
let g2 = gic_of_string "S A B; a b c; S; S -> a A; A -> a b c A | b B; B -> b c B | epsilon;";;
es_fnc g2;; (* Devuelve false *)

