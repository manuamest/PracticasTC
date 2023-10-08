#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

(*--------------------------------------EJERCICIO 1------------------------------------*)

(*estados; simbolos; estado inicial; estados finales; transiciones (e1 e2 s)*)

let automata = af_of_string "0 1 2 3; a b c; 0; 1 3;
0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c;";;

let automata2 = af_of_string "0 1 2 3; a b c; 0; 1 3;
 0 1 a; 0 2 a; 2 3 b; 3 2 a;";;

let automata3 = af_of_string "0 1 2 3; a b; 0; 3;
0 1 a; 0 2 b; 1 3 b; 1 2 a; 2 3 a; 2 1 b; 3 1 b; 3 0 a;"

(*Funcion es_afne(automata):
	Para cada estado en el automata:
		Si el simbolo de entrada de la transicion es epsilon:
			Devolver true
	Para cada estado en el automata:
		Para cada transcion en las transiciones del estado:
			Si el simbolo de entrada de la transicion no es epsilon:
				Si el estado de destino tiene epsilon-transiciones:
					Devolver true
	Devolver false*)

#load "str.cma";;

(*es_afne : Auto.af -> bool*)
let es_afne automata =
	let cadena  = string_of_af automata in
  let regexp_epsilon = Str.regexp "epsilon" in
  try
    let _ = Str.search_forward regexp_epsilon cadena 0 in
    true
  with Not_found -> false;;

(*Funcion es_afn(automata):
	Para cada estado en el automata:
		Para cada transicion en las transiciones del estado:
			Si hay algun simbolo de entrada duplicado en las transciones del estado:
				Devolver true
	Devolver false
*)

let rec no_determinismo arcos = match arcos with
	|[] -> false
	|Arco_af(e1, _, s)::resto ->
	if s = Terminal "" then
		no_determinismo resto
	else
		let arc_no_determinista = List.filter (fun (Arco_af(e1', _, s')) -> e1 = e1' && s = s') resto in
      if List.length arc_no_determinista > 0 then
        true
      else
        no_determinismo resto;;


(*es_afn : Auto.af -> bool*)
let es_afn (Af (estados, simbolos, e_ini, arcos, e_fin)) = 
	if es_afne (Af (estados, simbolos, e_ini, arcos, e_fin)) then false else
		match arcos with
	| Conjunto(l_of_arcos) -> no_determinismo l_of_arcos;;
	
	(*Funcion es_afd(automata)
		simbolos_entrada_inicial = lista de simbolos de entrada de las transiciones del estado inicial del automata
		estados_visitados = [estado inicial del automata]
		Para cada estado en estados_visitados:
			transiciones_estado = lista de transiciones del estado en el automata
			simbolos_entrada = lista de simbolos de entrada de las transiciones_estado
			Si hay algun simbolo de entrada duplicado en simbolos_entrada:
				Devolver false
			Para cada simbolo en simbolos_entrada:
				destino = entado de destino de la transicion correspondiente al simbolo
				Si destino no es un estado del automata
					Devolver false
				Si destino no esta en estados_visitados:
					añadir destino a estados_visitados
		Devolver true*)
		
		let es_afd (Af (estados, simbolos, e_ini, arcos, e_fin)) =
			if es_afn (Af (estados, simbolos, e_ini, arcos, e_fin)) then false
			else
				let e_list = match estados with Conjunto e -> e in
				let s_list = match simbolos with Conjunto s -> s in
				let a_list = match arcos with Conjunto a -> a in
				List.for_all (fun e ->
					List.for_all (fun s ->
						let transiciones = List.filter (fun (Arco_af(e1, _, s1)) -> e = e1 && s = s1) a_list in
						List.length transiciones = 1
					) s_list
				) e_list;;

		
(*---------------------------------------EJERCICIO 2---------------------------------*)

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

(*----------------------------------------EJERCICIO 3------------------------------------*)
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

