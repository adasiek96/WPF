(* Autor: Adam Sobecki; Recenzent: Marek Puzyna *)
(*==============================================*)

(* TYP ZŁĄCZALNEJ KOLEJKI PRIORYTETOWEJ *)
(*==============================================*)
type 'a queue =
  Node of ('a queue * 'a * 'a queue * int) | Null

(* PUSTA KOLEJKA PRIORYTETOWA *)
(*==============================================*)
let empty = Null

(* WYJĄTEK PODNOSZONY, GDY KOLEJKA JEST PUSTA *)
(*==============================================*)
exception Empty

(* FUNKCJE POMOCNICZE *)
(*==============================================*)
(* funkcja [height d] zwraca długość skrajnej
prawej ścieżki od korzenia do liścia *)
let height drzewo =
  match drzewo with
    | Null -> -1
    | Node (_, _, _, h) -> h
(* funkcja [value d] zwraca wartość znajdującą się
w węźle dla zadanego węzła *)
let value drzewo =
  match drzewo with
    | Null -> raise Empty
    | Node (_, v, _, _) -> v

(* FUNKCJE *)
(*==============================================*)
(* funkcja [is_empty] zwraca wartość [true], jeśli
dana kolejka jest pusta; w przeciwnym razie zwraca
wartość [false] *)
let is_empty drzewo = (drzewo = Null)
(* funkcja [join q1 q2] zwraca złączenie kolejek
[q1] i [q2] *)
let rec join d1 d2 =
  match (d1, d2) with
    | (Null, Null) -> Null
    | (Null, _) -> d2
    | (_, Null) -> d1
    | (Node (l, v, p, h), _) ->
      if value d1 > value d2 then join d2 d1
      else
        let d3 = join p d2
        in
          if height l > height d3
          then Node (l, v, d3, (height d3) + 1)
          else Node (d3, v, l, (height l) + 1)
(* funkcja [add e q] zwraca kolejkę powstałą
z dołączenia elementu [e] do kolejki [q] *)
let add v drzewo =
  join (Node (Null, v, Null, 0)) drzewo
(* funkcja [delete_min q] dla niepustej kolejki
zwraca parę [(e,q')], gdzie [e] jest elementem
minimalnym kolejki [q], a [q'] to [q] bez elementu
[e]; natomiast jeśli [q] jest puste podnosi
wyjątek [Empty] *)
let delete_min drzewo =
  match drzewo with
    | Null -> raise Empty
    | Node (l, v, p, h) -> (v, join l p)
;;
(* TESTY *)
(*==============================================*)
let drzewo1 =
  Node
   (Node (Node (Null, 6, Null, 0), 5,
      Node (Null, 10, Null, 0), 1), 2,
    Node (Node (Null, 13, Null, 0), 11,
      Node (Null, 12, Null, 0), 1), 2)

let drzewo2 =
 (Node (Node (Node (Null, 14, Null, 0), 9,
    Node (Null, 10, Null, 0), 1), 1,
      Node (Null, 4, Null, 0), 1))

let wynik12 =
  Node (Node
     (Node (Node (Null, 6, Null, 0), 5,
        Node (Null, 10, Null, 0), 1), 2,
      Node
       (Node (Node (Null, 13, Null, 0), 11,
          Node (Null, 12, Null, 0), 1), 4,
        Null, 0),
      1),
    1, Node (Node (Null, 14, Null, 0), 9,
      Node (Null, 10, Null, 0), 1), 2)

let drzewo3 =
  Node (Node (Null, 10, Null, 0), 5,
    Node (Null, 12, Null, 0), 1)

let drzewo4 =
  Node (Node (Node
     (Null, 14, Null, 0), 7, Null, 0), 3,
    Node (Null, 8, Null, 0), 1)

let wynik34 =
  Node (Node
     (Node (Node
       (Null, 12, Null, 0), 8, Null, 0), 5,
          Node (Null, 10, Null, 0), 1), 3,
    Node (Node
     (Null, 14, Null, 0), 7, Null, 0), 1)

let drzewo5 = Null

let v1 = 6
let v2 = 1
let v3 = 42

let wyn1 =
  Node (Node
   (Node (Null, 6, Null, 0), 5,
      Node (Null, 10, Null, 0), 1), 2,
    Node (Node
     (Node (Null, 13, Null, 0), 11,
        Node (Null, 12, Null, 0), 1), 6,
          Null, 0), 1)

let wyn2 =
  Node (Node
     (Node (Node (Null, 14, Null, 0), 9,
        Node (Null, 10, Null, 0), 1), 1,
      Node (Null, 4, Null, 0), 1), 1,
        Null, 0)

let wyn3 =
  Node (Null, 42, Null, 0)

let wynik =
  (5, Node (Node (Null, 12, Null, 0),
     10, Null, 0))
;;
(*==============================================*)
assert ((join drzewo1 drzewo2) = wynik12)
assert ((join drzewo3 drzewo4) = wynik34)

assert ((is_empty drzewo1) = false)
assert ((is_empty drzewo5) = true)

assert ((add v1 drzewo1) = wyn1)
assert ((add v2 drzewo2) = wyn2)
assert ((add v3 drzewo5) = wyn3)

assert ((delete_min drzewo3) = wynik)
(*assert ((delete_min drzewo5) = (raise Empty));;*)
(*==============================================*)

