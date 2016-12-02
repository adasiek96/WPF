(* I am making use of module FIND_UNION implemented below *)

module type FIND_UNION = sig
  type 'a set
  val make_set : 'a -> 'a set
  val find : 'a set -> 'a
  val equivalent : 'a set -> 'a set -> bool
  val union : 'a set -> 'a set -> unit
  val elements : 'a set -> 'a list
  val n_of_sets : unit -> int
end
;;
module Find_Union : FIND_UNION = struct
  type 'a set = {
    elem : 'a;
    up : 'a set ref;
    mutable rank : int;
    mutable next : 'a set list
  }

  let sets_counter = ref 0

  let n_of_sets () = !sets_counter

  let make_set x =
    let rec v = { elem = x; up = ref v; rank = 0; next = [] }
    in begin
      sets_counter := !sets_counter + 1;
      v
    end

  let rec go_up s =
    if s == !(s.up) then s
      else begin
        s.up := go_up !(s.up);
        !(s.up)
      end

  let find s =
    (go_up s).elem

  let equivalent s1 s2 =
    go_up s1 == go_up s2

  let union x y =
    let fx = go_up x
    and fy = go_up y
    in
      if not (fx == fy) then begin
        if fy.rank >fx.rank then begin
          fx.up := fy;
          fy.next <- fx :: fy.next
        end else begin
          fy.up := fx;
          fx.next <- fy :: fx.next;
          if fx.rank = fy.rank then fx.rank <- fy.rank + 1
        end;
        sets_counter := !sets_counter - 1
      end

  let elements s =
    let acc = ref []
    in
      let rec traverse s1 =
        begin
          acc := s1.elem :: !acc;
          List.iter traverse s1.next
        end
      in begin
        traverse (go_up s);
        !acc
      end
end
;;
open Find_Union
;;
let get x =
  match x with
  | Some (c) -> c
  | None -> assert false
;;
(* Zadanie 4. *)
let kolory l =
  let m =
    List.fold_left
      (fun a (x, y) ->
        (max (fst a) x, max (snd a) y))
      (0, 0) l
  in
    let tabx = Array.init (fst m + 1) (fun _ -> None)
    and taby = Array.init (snd m + 1) (fun _ -> None)
    and a = ref 1
    in
      List.iter
        (fun (x', y') ->
            if tabx.(x') = None
              then tabx.(x') <- Some (make_set a);
            if taby.(y') = None
              then taby.(y') <- Some (make_set a);
            union (get (tabx.(x'))) (get (taby.(y')));
            a := !a + 1) l;
        n_of_sets ()
;;
let l = [(1, 2); (1, 8); (3, 6)];;
kolory l;;
