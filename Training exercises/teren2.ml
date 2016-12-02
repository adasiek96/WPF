(* I am making use of module PRI_QUEUE implemented below *)

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

type stan = Nowy | Brzeg | Zalany
;;
exception Mam of int;;
let first (a, _, _) = a;;
let war (a, _, _) b = max (abs a) (abs b);;

let wysokosc ab n m =
  let h = ref ((ab.(0).(0)), 0, 0)
  and wyn = ref ab.(0).(0)
  and li = ref []
  and sasiad = [(0, 1); (1, 0); (-1, 0); (0, -1)]
  and q = ref empty_queue
  and tab = Array.make_matrix n m Nowy in
    let wrzuc (_, x, y) q =
      List.iter (fun (a, b) ->
        let x' = x + a in
          let y' = y + b in
            if x' = n - 1 && y' = m - 1
            then raise (Mam (max (ab.(x').(y')) !wyn)) else
              if x' >= 0 && y' >= 0 && x' <= n - 1 && y' <= m - 1 then
                if tab.(x').(y') = Nowy then
                  begin
                    tab.(x').(y') <- Brzeg;
                    q := put !q (-(ab.(x').(y')), x', y')
                  end
      ) sasiad
    in
      let zdejmij (_, x, y) q =
          h := getmax !q;
          wyn := war !h !wyn;
          li := !h :: !li;
          q := removemax !q;
          wrzuc !h q
      in
      wrzuc !h q;
      while not (is_empty !q) do
        zdejmij !h q
        done;
        !li
;;
let tab =
  Array.make_matrix 3 3 0;;
tab.(0).(0) <- 1;;
tab.(0).(1) <- 2;;
tab.(0).(2) <- 5;;
tab.(1).(0) <- 2;;
tab.(1).(1) <- 3;;
tab.(1).(2) <- 5;;
tab.(2).(0) <- 2;;
tab.(2).(1) <- 4;;
tab.(2).(2) <- 3;;
tab.(2).(2) <- 3;;
tab.(2).(2) <- 3;;
wysokosc tab 3 3;;

let tab2 =
  Array.make_matrix 6 5 0;;
tab2.(0).(0) <- 1;;
tab2.(0).(1) <- 8;;
tab2.(0).(2) <- 3;;
tab2.(0).(3) <- 1;;
tab2.(0).(4) <- 10;;
tab2.(1).(0) <- 1;;
tab2.(1).(1) <- 3;;
tab2.(1).(2) <- 4;;
tab2.(1).(3) <- 2;;
tab2.(1).(4) <- 10;;
tab2.(2).(0) <- 1;;
tab2.(2).(1) <- 2;;
tab2.(2).(2) <- 5;;
tab2.(2).(3) <- 9;;
tab2.(2).(4) <- 2;;
tab2.(3).(0) <- 2;;
tab2.(3).(1) <- 3;;
tab2.(3).(2) <- 4;;
tab2.(3).(3) <- 9;;
tab2.(3).(4) <- 1;;
tab2.(4).(0) <- 5;;
tab2.(4).(1) <- 4;;
tab2.(4).(2) <- 6;;
tab2.(4).(3) <- 8;;
tab2.(4).(4) <- 1;;
tab2.(5).(0) <- 1;;
tab2.(5).(1) <- 2;;
tab2.(5).(2) <- 7;;
tab2.(5).(3) <- 2;;
tab2.(5).(4) <- 1;;
wysokosc tab2 6 5;;
