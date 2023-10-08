(*EJERCICIO1---------------------------------------------------------------------*)
(*Funcion mapdoble*)

let mapdoble f g l =
  let rec aux i = function
      [] -> []
    | hd :: tl ->
        if i mod 2 = 0 then (f hd) :: (aux (i + 1) tl)
        else (g hd) :: (aux (i + 1) tl)
  in aux 0 l;;
 
(*Tipo de mapdoble*)
(*val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun>*)

(*Valor de mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;*)
(*Devuelve un error de tipos:
   Error: This expression has type string but an expression was expected of type int*)

(*Tipo de let y = function x -> 5 in mapdoble y;;*)
(*- : ('_weak1 -> int) -> '_weak1 list -> int list = <fun>*)
(*'_weak1 indica que es un tipo de datos debilmente tipado, es decir que 
el tipo de datos se conoce durante el tiempo de ejecucion pero no se especifica 
en tiempo de compilacion*)

(*EJERCICIO2---------------------------------------------------------------------*)
(*Funcion primero_que_cumple*)

let rec primero_que_cumple f l =
  match l with
  | [] -> raise Not_found
  | hd :: tl -> if f hd then hd else primero_que_cumple f tl;;

(*Para probarla: primero_que_cumple (fun x -> x mod 2 = 0) [1;2;3];;*)

(*Tipo de la funcion primero_que_cumple*)
(*val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun>*)

let existe f l = 
  try primero_que_cumple f l |> (fun _ -> true)
  with Not_found -> false;;

(*Para probarla: existe (fun x -> x mod 2 = 0) [1;2;3];;*)

(*Tipo de la funcion existe*)
(*val existe : ('a -> bool) -> 'a list -> bool = <fun>*)

(*Funcion asociado que devuelve el valor asociado a una clave*)
let asociado conjunto clave =
  let result = primero_que_cumple (fun (c,_) -> c = clave) conjunto in
  match result with
  | (c,v) -> v;;

(*Para probarla: asociado [("a",1);("b",2)] "a";;*)

(*EJERCICIO3---------------------------------------------------------------------*)
type 'a arbol_binario =
 Vacio
 | Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;

let rec in_orden = function
 Vacio -> []
 | Nodo (x, i, d) -> (in_orden i) @ [x] @ (in_orden d);;

let rec pre_orden = function
  Vacio -> []
  | Nodo (x, i, d) -> [x] @ (pre_orden i) @ (pre_orden d);;

let rec post_orden = function
  Vacio -> []
  | Nodo (x, i, d) -> (post_orden i) @ (post_orden d) @ [x];;

  let anchura arbol =
    let rec aux cola acum =
      match cola with
      | [] -> List.rev acum
      | Vacio :: tl -> aux tl acum
      | Nodo (x, i, d) :: tl -> aux (tl @ [i; d]) (x :: acum)   (*Insertamos los hijos al final de la cola*)
    in aux [arbol] [];;

(*Para probarlas: let t = Nodo (3, Nodo (2, Vacio, Vacio), Nodo (5, Nodo(4, Vacio, Vacio), Nodo(1, Vacio, Vacio)));;*)

(*EJERCICIO4---------------------------------------------------------------------*)
(*let conjunto = Conjunto [1; 2; 3; 4];;*)
type 'a conjunto = Conjunto of 'a list;;

let conjunto_vacio = Conjunto [];;

(*pertenece : 'a -> 'a conjunto -> bool*)
let pertenece x (Conjunto l) =
  List.exists (fun y -> x = y) l;;

(*agregar : 'a -> 'a conjunto -> 'a conjunto*)
let agregar x (Conjunto l) =
  if pertenece x (Conjunto l) 
    then (Conjunto l)
    else (Conjunto (x::l));;

(*conjunto_of_list : 'a list -> 'a conjunto*)
let conjunto_of_list l =
  let l = List.sort_uniq compare l in
  Conjunto l;;

(*suprimir : 'a -> 'a conjunto -> 'a conjunto*)
let suprimir x (Conjunto l) =
  Conjunto (List.filter (fun y -> x <> y) l);;

(*cardinal : 'a conjunto -> int*)
let cardinal (Conjunto l) =
  List.length l;;

(*union : 'a conjunto -> 'a conjunto -> 'a conjunto*)
let union (Conjunto l1) (Conjunto l2) =
  conjunto_of_list (l1 @ l2);;
  
(*interseccion : 'a conjunto -> 'a conjunto -> 'a conjunto*)
let interseccion (Conjunto l1) (Conjunto l2) =
  conjunto_of_list (List.filter (fun x -> pertenece x (Conjunto l2)) l1);;

(*diferencia : 'a conjunto -> 'a conjunto -> 'a conjunto*)
let diferencia (Conjunto l1) (Conjunto l2) =
  conjunto_of_list (List.filter (fun x -> not (pertenece x (Conjunto l2))) l1);;

(*incluido : 'a conjunto -> 'a conjunto -> bool*)
let incluido (Conjunto l1) (Conjunto l2) =
  List.for_all (fun x -> pertenece x (Conjunto l2)) l1;;

(*igual : 'a conjunto -> 'a conjunto -> bool*)
let igual (Conjunto l1) (Conjunto l2) =
  incluido (Conjunto l1) (Conjunto l2) && incluido (Conjunto l2) (Conjunto l1);;

(*producto cartesiano : 'a conjunto -> 'b conjunto -> ('a * 'b) conjunto*)
let producto_cartesiano (Conjunto l1) (Conjunto l2) =
  let rec iter l acc = match l with
    | [] -> acc
    | x::xs -> iter xs (acc @ List.map (fun y -> (x, y)) l2)
  in
  Conjunto (iter l1 []);;

(*lista_of_conjunto : 'a conjunto -> 'a list*)
let lista_of_conjunto (Conjunto l) =
  l;;
