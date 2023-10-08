#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

(*
Se comprueba que la gramática esté en forma normal de Chomsky utilizando la función es_fnc. Si no está en esta forma, se lanza una excepción InvalidInput.
Se comprueba que la cadena de entrada tenga al menos un símbolo. Si no es así, se lanza una excepción InvalidInput.
Se convierte la lista de símbolos de entrada en un array (entrada_arr) y se obtiene su longitud (longitud).
Se extraen las reglas de producción (producciones) y el símbolo inicial (simbolo_inicio) de la gramática.
Se crea una tabla de análisis (matriz) llamada tabla_cyk de tamaño longitud x longitud.
Se inicializa la diagonal principal de la tabla utilizando las reglas que generan directamente los símbolos terminales de la cadena de entrada.
Se llena el resto de la tabla utilizando las reglas de producción de la gramática. Para ello, se recorren las celdas de la tabla de arriba a abajo y de izquierda a derecha, considerando todas las posibles combinaciones de símbolos no terminales que pueden generar una subcadena de entrada.
Finalmente, se verifica si el símbolo inicial de la gramática está presente en la celda superior derecha de la tabla (tabla_cyk.(0).(longitud-1)). Si está presente, la cadena de entrada pertenece al lenguaje generado por la gramática y la función devuelve true. De lo contrario, devuelve false.
*)

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
