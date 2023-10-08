(*Funcion mapdoble*)

let mapdoble f g l =
  let rec aux i = function
      [] -> []
    | hd :: tl ->
        if i mod 2 = 0 then (f hd) :: (aux (i + 1) tl)
        else (g hd) :: (aux (i + 1) tl)
  in
  aux 0 l;;

(*Tipo de mapdoble*)
(*val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun>*)

(*Valor de mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;*)
(*Devuelve un error de tipos*)

(*Tipo de let y = function x -> 5 in mapdoble y;;*)
(*- : ('_weak1 -> int) -> '_weak1 list -> int list = <fun>*)
(*'_weak1 indica que es un tipo de datos debilmente tipado, es decir que 
el tipo de datos se conoce durante el tiempo de ejecucion pero no se especifica 
en tiempo de compilacion*)