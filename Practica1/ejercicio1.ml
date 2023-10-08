#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

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

		
		