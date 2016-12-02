(* I am making use of modules implemented below *)

open List;;

module type PRI_QUEUE = sig
  type 'a pri_queue
  val empty_queue : 'a pri_queue
  val is_empty : 'a pri_queue -> bool
  val put : 'a pri_queue -> 'a -> 'a pri_queue
  val getmax : 'a pri_queue -> 'a
  val removemax : 'a pri_queue -> 'a pri_queue
  exception Empty_Queue
end;;

module Heap_Pri_Queue : PRI_QUEUE = struct

  exception Empty_Queue 

  type 'a pri_queue =
    Node of 'a * 'a pri_queue * 'a pri_queue * int | Null

  let empty_queue = Null

  let is_empty q = q = Null

  let size q =
    match q with
      | Null -> 0
      | Node (_, _, _, n) -> n

  let getmax h =
    match h with
      | Null -> raise Empty_Queue
      | Node (r, _, _, _) -> r

  let set_root h r =
    match h with
      | Null -> Node (r, Null, Null, 1)
      | Node (_, l, p, n) -> Node (r, l, p, n)

  let rec put h x =
    match h with
      | Null -> Node (x, Null, Null, 1)
      | Node (r, l, p, n) ->
        if size l <= size p
        then Node ((max x r), (put l (min x r)), p, (n + 1))
        else Node ((max x r), l, (put p (min x r)), (n + 1))

  let rec removemax h =
    match h with
      | Null -> raise Empty_Queue
      | Node (_, Null, Null, _) -> Null
      | Node (_, l, Null, _) -> l
      | Node (_, Null, p, _) -> p
      | Node (_, (Node (rl, _, _, _) as l), (Node (rp, _, _, _) as p), n) ->
        if rl >= rp
        then Node (rl, removemax l, p, n - 1)
        else Node (rp, l, removemax p, n - 1)

end;;

open Heap_Pri_Queue;;

let zlewki lista k =
  let kolejka =
    fold_left (fun a x -> put a (-.x)) empty_queue lista in
      let rec proc q k =
        if k = 0 then (-.(getmax q)) else
          let a = getmax q in
            let q = removemax q in
              let b = getmax q in
                let q = removemax q in
                  proc (put q (a +. b)) (k - 1)
      in proc kolejka k
;;

let katastrofy l =
  let rec znajdz (w, (s, i)) h =
    match s with
      | [] -> ([i], ([(h, i)], i + 1))
      | (x, j) :: t ->
        if x > h
        then ((i - j) :: w, ((h, i) :: s, i + 1))
        else znajdz (w, (t, i)) h
  in rev (fst (fold_left znajdz ([], ([], 1)) l))
;;

let roznica lista =
0
;;

let lista = [100;1;2;8;5;3;4];;
katastrofy lista;;

let roznica l =
  if l = [] then (0, 0) else
    let (lista1, lista2, _, _) =
      fold_left
        (fun (lista1, lista2, v, i) x ->
          if x >= v then (lista1, (x, i) :: lista2, v, i + 1)
          else ((x, i) :: lista1, lista2, x, i + 1))
        ([(hd l, 1)], [], hd l, 2) (tl l)
    in
      let rec pom lista1 lista2 (i, j) =
        match lista1, lista2 with
          | (h1, i') :: t1, (h2, j') :: t2 ->
            if h2 >= h1 then
              if j' - i' > j - i then pom t1 lista2 (i', j')
              else pom t1 lista2 (i, j)
            else
              pom lista1 t2 (i, j)
          | _ -> (i, j)
      in pom lista1 lista2 (0, 0)
;;

let l1 = [9; -5; -2; -3; -4; 1; 6; -3; -5; -5];;
let l2 = [8; 9; 8; 0; 7; 4; -3; -7; -9; -9];;
let l3 = [4; 6; 2; 1; -5; 8; 2; -4; 0; -7];;
let l4 = [7; 2; -1; -9; -4; -5; -10; -6; 0; -2];;
let l5 = [1; 2; -4; 5; 3; 8; -3; 2; -4; 0; -10; -5; -6];;
roznica l1;;
roznica l2;;
roznica l3;;
roznica l4;;
roznica l5;;












open List;;

module type PRI_QUEUE = sig
  type 'a pri_queue
  val empty_queue : 'a pri_queue
  val is_empty : 'a pri_queue -> bool
  val put : 'a pri_queue -> 'a -> 'a pri_queue
  val getmax : 'a pri_queue -> 'a
  val removemax : 'a pri_queue -> 'a pri_queue
  exception Empty_Queue
end;;

module Heap_Pri_Queue : PRI_QUEUE = struct

  exception Empty_Queue 

  type 'a pri_queue =
    Node of 'a * 'a pri_queue * 'a pri_queue * int | Null

  let empty_queue = Null

  let is_empty q = q = Null

  let size q =
    match q with
      | Null -> 0
      | Node (_, _, _, n) -> n

  let getmax h =
    match h with
      | Null -> raise Empty_Queue
      | Node (r, _, _, _) -> r

  let set_root h r =
    match h with
      | Null -> Node (r, Null, Null, 1)
      | Node (_, l, p, n) -> Node (r, l, p, n)

  let rec put h x =
    match h with
      | Null -> Node (x, Null, Null, 1)
      | Node (r, l, p, n) ->
        if size l <= size p
        then Node ((max x r), (put l (min x r)), p, (n + 1))
        else Node ((max x r), l, (put p (min x r)), (n + 1))

  let rec removemax h =
    match h with
      | Null -> raise Empty_Queue
      | Node (_, Null, Null, _) -> Null
      | Node (_, l, Null, _) -> l
      | Node (_, Null, p, _) -> p
      | Node (_, (Node (rl, _, _, _) as l), (Node (rp, _, _, _) as p), n) ->
        if rl >= rp
        then Node (rl, removemax l, p, n - 1)
        else Node (rp, l, removemax p, n - 1)

end;;

open Heap_Pri_Queue;;

let zlewki lista k =
  let kolejka =
    fold_left (fun a x -> put a (-.x)) empty_queue lista in
      let rec proc q k =
        if k = 0 then (-.(getmax q)) else
          let a = getmax q in
            let q = removemax q in
              let b = getmax q in
                let q = removemax q in
                  proc (put q (a +. b)) (k - 1)
      in proc kolejka k
;;

let katastrofy l =
  let rec znajdz (w, (s, i)) h =
    match s with
      | [] -> ([i], ([(h, i)], i + 1))
      | (x, j) :: t ->
        if x > h
        then ((i - j) :: w, ((h, i) :: s, i + 1))
        else znajdz (w, (t, i)) h
  in rev (fst (fold_left znajdz ([], ([], 1)) l))
;;

let roznica lista =
0
;;

let lista = [100;1;2;8;5;3;4];;
katastrofy lista;;

let roznica l =
  if l = [] then (0, 0) else
    let (lista1, lista2, _, _) =
      fold_left
        (fun (lista1, lista2, v, i) x ->
          if x >= v then (lista1, (x, i) :: lista2, v, i + 1)
          else ((x, i) :: lista1, lista2, x, i + 1))
        ([(hd l, 1)], [], hd l, 2) (tl l)
    in
      let rec pom lista1 lista2 (i, j) =
        match lista1, lista2 with
          | (h1, i') :: t1, (h2, j') :: t2 ->
            if h2 >= h1 then
              if j' - i' > j - i then pom t1 lista2 (i', j')
              else pom t1 lista2 (i, j)
            else
              pom lista1 t2 (i, j)
          | _ -> (i, j)
      in pom lista1 lista2 (0, 0)
;;

let l1 = [9; -5; -2; -3; -4; 1; 6; -3; -5; -5];;
let l2 = [8; 9; 8; 0; 7; 4; -3; -7; -9; -9];;
let l3 = [4; 6; 2; 1; -5; 8; 2; -4; 0; -7];;
let l4 = [7; 2; -1; -9; -4; -5; -10; -6; 0; -2];;
let l5 = [1; 2; -4; 5; 3; 8; -3; 2; -4; 0; -10; -5; -6];;
roznica l1;;
roznica l2;;
roznica l3;;
roznica l4;;
roznica l5;;












