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

(* Funcion cyk *)
exception InvalidInput

let cyk (input: Auto.simbolo list) (gic: Auto.gic) : bool =
  if not (es_fnc gic) then raise InvalidInput;
  if List.length input < 1 then raise InvalidInput;
  let input_arr = Array.of_list input in
  let n = Array.length input_arr in
  let Gic (_, _, reglas, inicio) = gic in
  let cyk_table = Array.make_matrix n n [] in
  for i = 0 to n - 1 do
    cyk_table.(i).(i) <- List.fold_left (fun acc (Regla_gic (lhs, rhs)) ->
      if List.hd rhs = input_arr.(i) then lhs::acc else acc) [] (list_of_conjunto reglas);
  done;
  for j = 1 to n - 1 do
    for i = 0 to n - j - 1 do
      for k = 0 to j - 1 do
        cyk_table.(i).(i+j) <- List.fold_left (fun acc (Regla_gic (lhs, rhs)) ->
          match rhs with
          | [b; c] when List.mem b cyk_table.(i).(i+k) && List.mem c cyk_table.(i+k+1).(i+j) -> lhs::acc
          | _ -> acc) cyk_table.(i).(i+j) (list_of_conjunto reglas);
      done;
    done;
  done;
  List.mem inicio cyk_table.(0).(n-1);;

(* Definimos la gramática g1 *)
let g1 = gic_of_string "S A B; a b; S; S -> A B; A -> a; B -> b;";;

(* Ejemplo 1: Devuelve true *)
let input1 = [Terminal "a"; Terminal "b"];;
let result1 = cyk input1 g1;;
Printf.printf "Resultado 1: %B\n" result1;;  (* Resultado 1: true *)

exception InvalidInput

let cyk (input: Auto.simbolo list) (gic: Auto.gic) : bool =
  if not (es_fnc gic) then raise InvalidInput;
  if List.length input < 1 then raise InvalidInput;
  let input_arr = Array.of_list input in
  let n = Array.length input_arr in
  let Gic (_, _, reglas, inicio) = gic in
  let cyk_table = Array.make_matrix n n [] in
  for i = 0 to n - 1 do
    cyk_table.(i).(i) <- List.fold_left (fun acc (Regla_gic (lhs, rhs)) ->
      if List.hd rhs = input_arr.(i) then lhs::acc else acc) [] (list_of_conjunto reglas);
  done;
  for j = 1 to n - 1 do
    for i = 0 to n - j - 1 do
      for k = 0 to j - 1 do
        cyk_table.(i).(i+j) <- List.fold_left (fun acc (Regla_gic (lhs, rhs)) ->
          match rhs with
          | [b; c] when List.mem b cyk_table.(i).(i+k) && List.mem c cyk_table.(i+k+1).(i+j) -> lhs::acc
          | _ -> acc) cyk_table.(i).(i+j) (list_of_conjunto reglas);
      done;
    done;
  done;
  List.mem inicio cyk_table.(0).(n-1);;

(* Definimos la gramática g1 *)
let g1 = gic_of_string "S A B; a b; S; S -> A B; A -> a; B -> b;";;

(* Ejemplo 1: Devuelve true *)
let input1 = [Terminal "a"; Terminal "b"];;
let result1 = cyk input1 g1;;
Printf.printf "Resultado 1: %B\n" result1;;  (* Resultado 1: true *)

(* Ejemplo 2: Devuelve false *)
let input2 = [Terminal "a"; Terminal "a"];;
let result2 = cyk input2 g1;;
Printf.printf "Resultado 2: %B\n" result2;;  (* Resultado 2: false *)

(* Ejemplo de los apuntos: Devuelve true*)
let g2 = gic_of_string "S A B C; a b; S; S -> A B | B C; A -> B A | a; B -> C C | b; C -> A B | a;";;

let cadena = [Terminal "b"; Terminal "b"; Terminal "a"; Terminal "b"];;

(* Ejemplo 2: Devuelve false *)
let input2 = [Terminal "a"; Terminal "a"];;
let result2 = cyk input2 g1;;
Printf.printf "Resultado 2: %B\n" result2;;  (* Resultado 2: false *)

(* Ejemplo de los apuntos: Devuelve true*)
let g2 = gic_of_string "S A B C; a b; S; S -> A B | B C; A -> B A | a; B -> C C | b; C -> A B | a;";;

let cadena = [Terminal "b"; Terminal "b"];;
