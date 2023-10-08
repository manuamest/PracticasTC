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
