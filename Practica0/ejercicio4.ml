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
  conjunto_of_list (List.map (fun x -> List.map (fun y -> (x,y)) l2) l1 |> List.flatten);;

(*lista_of_conjunto : 'a conjunto -> 'a list*)
let lista_of_conjunto (Conjunto l) =
  l;;