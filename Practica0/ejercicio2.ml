(*Funcion primero_que_cumple*)

let rec primero_que_cumple f l =
  match l with
  | [] -> raise Not_found
  | hd :: tl -> if f hd then hd else primero_que_cumple f tl;;

(*Para probarla: primero_que_cumple (fun x -> x mod 2 = 0) [1;2;3];;*)

(*Tipo de la funcion primero_que_cumple*)
(*val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun>*)

let rec existe f l =
  match l with
  | [] -> false
  | hd :: tl -> if f hd then true else existe f tl;;

(*Para probarla: existe (fun x -> x mod 2 = 0) [1;2;3];;*)

(*Tipo de la funcion existe*)
(*val existe : ('a -> bool) -> 'a list -> bool = <fun>*)

(*Funcion asociado que devuelve el valor asociado a una clave*)
let asociado conjunto clave = 
  primero_que_cumple (fun (c,_) -> c = clave) conjunto
  |> (fun (_,v) -> v);;

(*Para probarla: asociado [("a",1);("b",2)] "a";;*)