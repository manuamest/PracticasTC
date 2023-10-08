#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

let automata = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c;";;

let automata2 = af_of_string "0 1 2 3; a b c; 0; 1 3;
 0 1 a; 0 2 a; 2 3 b; 3 2 a;";;

let automata3 = af_of_string "0 1 2 3; a b; 0; 3;
0 1 a; 0 2 b; 1 3 b; 1 2 a; 2 3 a; 2 1 b; 3 1 b; 3 0 a;";;

let escaner_af cadena (Af (_, _, inicial, _, finales) as a) =

  let rec aux = function

       (Conjunto [], _) ->
          false

     | (actuales, []) ->
          not (es_vacio (interseccion actuales finales))

     | (actuales, simbolo :: t) ->
          aux ((epsilon_cierre (avanza simbolo actuales a) a), t)

  in
     aux ((epsilon_cierre (Conjunto [inicial]) a), cadena)
;;

(* Escáner optimizado para AFN (sin épsilon-transiciones) *)
let escaner_afn cadena (Af (_, _, inicial, _, finales) as a) =
  let rec aux estados = function
    | [] ->
      List.exists (fun estado -> pertenece estado finales) estados
    | simbolo :: t ->
      let nuevos_estados =
        List.fold_left
          (fun acum estado ->
            union acum (avanza simbolo (Conjunto [estado]) a))
          conjunto_vacio estados
      in
      aux (list_of_conjunto nuevos_estados) t
  in
  aux [inicial] cadena
;;

(* Escáner optimizado para AFD *)
let escaner_afd cadena (Af (_, _, inicial, _, finales) as a) =
  let rec aux estado = function
    | [] ->
      pertenece estado finales
    | simbolo :: t ->
      let nuevos_estados = avanza simbolo (Conjunto [estado]) a in
      if cardinal nuevos_estados = 1 then
        aux (List.hd (list_of_conjunto nuevos_estados)) t
      else
        false
  in
  aux inicial cadena
;;

let cadena1 = [Terminal "a"; Terminal "b"; Terminal "a"; Terminal "c"]
let resultado1 = escaner_af cadena1 automata
let resultado2 = escaner_af cadena1 automata2
let resultado3 = escaner_af cadena1 automata3

(* Utilizando la función escaner_afn *)
let cadena2 = [Terminal "a"; Terminal "b"; Terminal "a"; Terminal "b"]
let resultado4 = escaner_afn cadena2 automata
let resultado5 = escaner_afn cadena2 automata2
let resultado6 = escaner_afn cadena2 automata3

(* Utilizando la función escaner_afd *)
let cadena3 = [Terminal "a"; Terminal "a"; Terminal "a"]
let resultado7 = escaner_afd cadena3 automata
let resultado8 = escaner_afd cadena3 automata2
let resultado9 = escaner_afd cadena3 automata3

(* Imprimir resultados *)
let () =
  Printf.printf "escaner_af resultados: %b %b %b\n" resultado1 resultado2 resultado3;
  Printf.printf "escaner_afn resultados: %b %b %b\n" resultado4 resultado5 resultado6;
  Printf.printf "escaner_afd resultados: %b %b %b\n" resultado7 resultado8 resultado9;;
