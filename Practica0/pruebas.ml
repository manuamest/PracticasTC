(*EJERCICIO1*)
(*Funcion mapdoble*)

(*Caso 1*)
let f1 = function x -> x*2;;
let g1 = function x -> x+2;;
let l = [1;2;3;4;5];;
assert (mapdoble f1 g1 l = [2; 4; 6; 6; 10]);;

(*Caso 2*)
let f2 = function x -> x*3;;
let g2 = function x -> x-2;;
assert (mapdoble f2 g2 [] = []);;

(*EJERCICIO2*)
(*Funcion primero_que_cumple*)

let f1 = function x -> x mod 2 = 0;;
assert (primero_que_cumple f1 [1;2;3;4] = 2);;

existe f1 [1;2;3];;

asociado [("a",1);("b",2)] "a";;

asociado [("a", 1);("b", 2)] "c";;

(*EJERCICIO3*)
(*Funciones para arboles binarios*)

(*Caso 1*)
assert (in_orden t1 = [2; 3; 4; 5; 1]);;
assert (pre_orden t1 = [3; 2; 5; 4; 1]);;
assert (post_orden t1 = [2; 4; 1; 5; 3]);;
assert (anchura t1 = [3; 2; 5; 4; 1]);;let t1 = Nodo (3, Nodo (2, Vacio, Vacio), Nodo (5, Nodo(4, Vacio, Vacio), Nodo(1, Vacio, Vacio)));;


(*Caso 2*)
let t2 = Nodo (2, Nodo (5, Nodo(4, Nodo(6, Vacio, Vacio), Vacio), Nodo(1, Vacio, Nodo(7, Vacio, Vacio))), Nodo (3, Vacio, Vacio));;
assert (in_orden t2 = [6; 4; 5; 1; 7; 2; 3]);;
assert (pre_orden t2 = [2; 5; 4; 6; 1; 7; 3]);;
assert (post_orden t2 = [6; 4; 7; 1; 5; 3; 2]);;
assert (anchura t2 = [2; 5; 3; 4; 1; 6; 7]);;

(*EJERCICIO4*)

(* Prueba para conjunto_vacio *)
  assert (conjunto_vacio = Conjunto []);;

(* Prueba para pertenece *)
  let conjunto = Conjunto [1; 2; 3; 4];;
  assert (pertenece 2 conjunto = true);;
  assert (pertenece 5 conjunto = false);;

(* Prueba para agregar *)
  let conjunto = Conjunto [1; 2; 3];;
  let conjunto_con_elemento = agregar 4 conjunto in
  assert (conjunto_con_elemento = Conjunto [4; 1; 2; 3]);;
  let conjunto_sin_cambios = agregar 3 conjunto in
  assert (conjunto_sin_cambios = conjunto);;

(* Prueba para conjunto_of_list *)
  let conjunto = conjunto_of_list [3; 1; 4; 1; 5; 9; 2; 6; 5; 3] in
  assert (conjunto = Conjunto [1; 2; 3; 4; 5; 6; 9]);;

(* Prueba para suprimir *)
  let conjunto = Conjunto [1; 2; 3; 4] in
  let conjunto_sin_elemento = suprimir 2 conjunto in
  assert (conjunto_sin_elemento = Conjunto [1; 3; 4]);
  let conjunto_sin_cambios = suprimir 5 conjunto in
  assert (conjunto_sin_cambios = conjunto);;

(* Prueba para cardinal *)
  let conjunto = Conjunto [1; 2; 3; 4] in
  assert (cardinal conjunto = 4);
  let conjunto_vacio = Conjunto [] in
  assert (cardinal conjunto_vacio = 0);;

(* Prueba para union *)
  let conjunto1 = Conjunto [1; 2; 3] in
  let conjunto2 = Conjunto [2; 3; 4] in
  let union_conjuntos = union conjunto1 conjunto2 in
  assert (union_conjuntos = Conjunto [1; 2; 3; 4]);;

(* Prueba para interseccion *)
  let conjunto1 = Conjunto [1; 2; 3; 4] in
  let conjunto2 = Conjunto [3; 4; 5; 6] in
  let interseccion_conjuntos = interseccion conjunto1 conjunto2 in
  assert (interseccion_conjuntos = Conjunto [3; 4]);;

(* Prueba para diferencia *)
  let conjunto1 = Conjunto [1; 2; 3; 4] in
  let conjunto2 = Conjunto [3; 4; 5; 6] in
  let diferencia_conjuntos = diferencia conjunto1 conjunto2 in
  assert (diferencia_conjuntos = Conjunto [1; 2]);;

(* Prueba de incluido *)
let c1 = conjunto_of_list [1;2;3];;
let c2 = conjunto_of_list [1;2;3;4;5];;
let c3 = conjunto_of_list [1;2;4;5];;
assert (incluido c1 c2 = true);;
assert (incluido c1 c3 = false);;

(* Prueba de igual *)
let c4 = conjunto_of_list [3;1;2];;
assert (igual c1 c4 = true);;
assert (igual c1 c2 = false);;

(* Prueba de producto_cartesiano *)
let c5 = conjunto_of_list [1;2];;
let c6 = conjunto_of_list ['a';'b'];;
let c7 = producto_cartesiano c5 c6;;
let expected = conjunto_of_list [(1,'a');(1,'b');(2,'a');(2,'b')];;
assert (igual c7 expected);;

(* Prueba de lista_of_conjunto *)
let l1 = [1;2;3];;
let c8 = conjunto_of_list l1;;
assert (lista_of_conjunto c8 = l1);;