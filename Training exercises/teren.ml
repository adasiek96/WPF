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
exception Koniec;;

let wysokosc tab1 =
  let n = Array.length tab1
  and m = Array.length tab1.(0) in
    let wyn = ref tab1.(0).(0)
    and sasiad = [(0, 1); (1, 0); (-1, 0); (0, -1)]
    and q = ref (put empty_queue (-tab1.(0).(0), 0, 0))
    and h = ref (0, 0, 0)
    and tab2 = Array.make_matrix n m false in
      let wrzuc (_, x, y) q =
        List.iter (fun (a, b) ->
          let x' = x + a
          and y' = y + b in
            if x' = n - 1 && y' = m - 1 then
              begin
                wyn := (max (tab1.(x').(y')) !wyn);
                raise Koniec
              end
            else
              if x' >= 0 && y' >= 0 && x' <= n - 1 && y' <= m - 1 then
                if tab2.(x').(y') = false then
                  begin
                    tab2.(x').(y') <- true;
                    q := put !q (-(tab1.(x').(y')), x', y')
                  end
        ) sasiad
      in (try
        while not (is_empty !q) do
          h := getmax !q;
          wyn := (let (w, _, _) = !h in max (abs w) !wyn);
          q := removemax !q;
          wrzuc !h q
        done
      with Koniec -> ());
      !wyn
;;
let dane1 =
  [|[|1; 2; 5|]; [|2; 3; 5|]; [|2; 4; 3|]|];;
let dane2 =
  [|[|1; 8; 3; 1; 10|]; [|1; 3; 4; 2; 10|]; [|1; 2; 5; 9; 2|];
  [|2; 3; 4; 9; 1|]; [|5; 4; 6; 8; 1|]; [|1; 2; 7; 2; 1|]|];;
assert (wysokosc dane1 = 4);;
assert (wysokosc dane2 = 7);;
