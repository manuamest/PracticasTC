#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;
 

let afd1 = af_of_string "0 1 2 3; a b; 0; 0;
0 1 a; 0 2 b; 1 0 a; 1 3 b; 2 3 a; 2 0 b; 3 2 a; 3 1 b;" ;;

let afd2 = af_of_string "A B C D; a b; A; A;
A B a; A C b; B A a; B D b; C D a; C A b; D C a; D B b;" ;;

let afn1 = af_of_string "0 1 2 3 4; a b; 0; 4;
0 1 a; 0 2 b; 1 3 b; 1 3 a; 2 3 a; 2 3 b; 3 4 a; 3 4 b;" ;;

let afn2 = af_of_string "S X Y Z T; a b; S; T;
S X a; S Y b; X Z b; X Z a; Y Z a; Y Z b; Z T a; Z T b;" ;;


(*
equivalente(automata 1, automata 2) = 
   estados1=estados del aut1
   estados_finales1=estados finales del autómata1
   estados2=estados del aut2
   estados_finales2=estados finales del automata2
   alfabeto=alfabeto del automata 1 y 2
   estados_visitados= conjunto vacío
   cola=[(estado_inicial1, estado_inicial2)]
   Mientras cola no esté vacía:
      (estado_actual1, estado_actual2) = extraer el primer elemento de la cola
      si (estado_actual1, estado_actual2) está en visitados:
          Continuar con el siguiente elemento de la cola
      si estado_actual1 es final y estado_actual2 no es final, o viceversa:
          Devolver false
      si no:
          añadir (estado_actual1, estado_actual2) a estados visitados
          Para cada simbolo en el alfabeto:
              nuevo_estado1=estado alcanzado desde el estado_actual1 com el simbolo
              nuevo_estado2=estado alcanzado desde el estado_actual2 con el simbolo
              *Añadir(nuevo_estado1,nuevo_estado2) a la cola
Devolver true
*)

let renombrar_estados_af prefijo (Af (estados, alfabeto, inicial, arcos, finales)) =
  let renombrar_estado (Estado s) = Estado (prefijo ^ s) in
  let renombrar_arco (Arco_af (origen, destino, simbolo)) =
    Arco_af (renombrar_estado origen, renombrar_estado destino, simbolo)
  in
  Af (conjunto_of_list (List.map renombrar_estado (list_of_conjunto estados)),
      alfabeto,
      renombrar_estado inicial,
      conjunto_of_list (List.map renombrar_arco (list_of_conjunto arcos)),
      conjunto_of_list (List.map renombrar_estado (list_of_conjunto finales)));;

let simbolo_of_af (Af (_, alfabeto, _, _, _)) = alfabeto;;

let equivalentes (af1 : af) (af2 : af) =
  let af2_renombrado = renombrar_estados_af "af2_" af2 in
  equivalentes_sin_renombrar af1 af2_renombrado;;

let equivalentes_sin_renombrar (af1 : af) (af2 : af) =
  let rec explorar cola visitados =
    match cola with
    | [] -> true
    | (estado1, estado2) :: resto ->
      if pertenece (estado1, estado2) visitados then
        explorar resto visitados
      else
        let visitados2 = agregar (estado1, estado2) visitados in
        let epsilon_cierre1 = epsilon_cierre (Conjunto [estado1]) af1 in
        let epsilon_cierre2 = epsilon_cierre (Conjunto [estado2]) af2 in
        if not (cardinal epsilon_cierre1 = cardinal epsilon_cierre2) then
          false
        else
          let simbolos = union (simbolo_of_af af1) (simbolo_of_af af2) in
          let nuevos_pares, son_equiv = List.fold_left
            (fun (acum, equiv) simbolo ->
              let avanza1 = avanza simbolo epsilon_cierre1 af1 in
              let avanza2 = avanza simbolo epsilon_cierre2 af2 in
              if cardinal avanza1 = cardinal avanza2 then
                (List.fold_left2
                   (fun acum' e1 e2 -> agregar (e1, e2) acum')
                   acum (list_of_conjunto avanza1) (list_of_conjunto avanza2), equiv)
              else
                (acum, false))
            (conjunto_vacio, true) (list_of_conjunto simbolos)
          in
          if son_equiv then
            explorar (resto @ (list_of_conjunto nuevos_pares)) visitados2
          else
            false
  in
  let Af (_, _, inicial1, _, _) = af1 in
  let Af (_, _, inicial2, _, _) = af2 in
  explorar [(inicial1, inicial2)] conjunto_vacio
;;
