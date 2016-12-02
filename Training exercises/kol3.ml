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

let wyspa tab1 =
  let m = Array.length tab1
  and n = Array.length tab1.(0) in
    let tab2 = Array.make_matrix (m + 2) (n + 2) 0 in
      for i = 1 to m do
        for j = 1 to n do
          tab2.(i).(j) <- -1
        done
      done;
      let sasiad_woda = [(0, 1); (1, 0); (0, -1); (-1, 0);
        (1, 1); (-1, -1); (1, -1); (-1, 1)]
      and sasiad_lad = [(0, 1); (1, 0); (0, -1); (-1, 0)]
      and wyn = ref 0
      and lista1 = ref [tab2.(0).(0)]
      and lista2 = ref []
      and licznik = ref 0
      in
        let wrzuc sasiad war_bool (x, y) lista lista_druga =
          List.iter (fun (a, b) ->
            let x' = x + a
            and y' = y + b in
              if x' >= 0 && y' >= 0 && x' <= m + 1 && y' <= n + 1 then
                if tab2.(x').(y') = -1 then
                  if tab1.(x').(y') = war_bool then
                    begin
                      tab2.(x').(y') <- !wyn;
                      licznik := !licznik + 1;
                      lista := tab2.(x').(y') :: !lista
                    end
                  else
                    lista_druga := tab2.(x').(y') :: !lista_druga
          ) sasiad
          in
          while true do
            while lista1 != [] do
              wrzuc sasiad_woda false (List.hd lista1) lista1 lista2;
            done;
            wyn := !wyn + 1;
            while lista2 != [] do
              wrzuc sasiad_lad false (List.hd lista1) lista2 lista1;
            done
          done
;;
  











