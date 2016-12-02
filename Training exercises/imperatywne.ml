(* Zadanie 1. *)
module type COUNTER =
sig
  type counter
  val make : unit -> counter
  val inc : counter -> int
  val reset : unit -> unit
end
;;
(* Rozwiązanie 1. *)
module Counter : COUNTER =
struct
  type counter = int ref
  let counter_list = ref []
  let make () =
    let a = ref 0
    in counter_list := a :: !counter_list; a
  let inc a = a := !a + 1; !a
  let reset () = List.iter (fun x -> x := 0) ! counter_list
end
;;
(* Rozwiązanie 2. *)
module Counter : COUNTER =
struct
  type counter = int ref
  let counter_list = ref []
  let make () = ref 0
  let inc a = a:= !a + 1;
    if !a = 1 then counter_list := a :: !counter_list; !a
  let reset () = List.iter (fun x -> x := 0) !counter_list;
    counter_list := []
end
;;
(* Rozwiązanie 3. *)
module Counter : COUNTER =
struct
  type counter = (int * int) ref
  let counter_time = ref 0
  let make () = ref (0, !counter_time)
  let inc a =
    match !a with (war, time) ->
      if time < !counter_time
      then a:= (1, !counter_time)
      else a := (war + 1, time); fst !a
  let reset () = counter_time := !counter_time + 1
end
;;

(* Zadanie 2. *)
type 'a drzewo =
  | Wezel of 'a * 'a drzewo * 'a drzewo * 'a drzewo ref
  | Puste
;;
(* Rozwiązanie *)
let fastryguj t =
  let rec pom t o =
    match t with
      | Puste -> o
      | Wezel (_, l, r, v) ->
        v := pom r o; pom l t
  in ignore (pom t Puste)
;;
let drzewko =
  Wezel (7,
   Wezel (5, Puste, Wezel (2, Puste, Puste, {contents = Puste}),
    {contents = Puste}),
   Wezel (3, Wezel (1, Puste, Puste, {contents = Puste}), Puste,
    {contents = Puste}),
   {contents = Puste})
;;
let drzewko2 = 
  Wezel (5, Puste, Wezel (2, Puste, Puste, ref Puste), ref Puste)
;;
fastryguj drzewko;; fastryguj drzewko2;; drzewko2;;

(* Zadanie 3. *)
let minmax a =
  let d = Array.length a in
    let rec szukaj i j =
      let s = (i + j) / 2 in
        if a.(s) < a.(0) then
          if a.(s - 1) > a.(0) then (s, s - 1)
          else szukaj i s
        else szukaj (s + 1) j
    in
      if d <= 1 then (0, 0) else
        if a.(0) < a.(d - 1) then (0, d - 1)
        else szukaj 1 (d - 1)
;;

(* Zadanie 4a. *)
let rozne a =
  let i = ref 0
  and j = ref (Array.length a - 1)
  and b = ref true
  in
    while not (!i = !j) && !b do
      if a.(!i) = a.(!j) then b := false else
        if a.(!i) < a.(!j) then j := !j - 1 else i := !i + 1
    done;
    !b
;;
let a1 = [|9; 4; 2; -1; -1; 3; 4; 6|];;
let a2 = [|9; 4; 2; 1; 3; 6|];;
rozne a1;;
rozne a2;;

(* Zadanie 4b. *)
let minimum tab =
  let i = ref 0
  and j = ref (Array.length tab - 1)
  and s = ref 0
  in
    while !i < !j do
      s := (!i + !j) / 2;
      if tab.(!s) > tab.(!s + 1)
      then i := !s + 1
      else j := !s
      done;
      tab.(!i)
;;
minimum a1
;;
(* bin_search *)
(* dla tablicy malejącej; dla rosnącej > zamieniamy na < *)
let bin_search1 x tab =
  let i = ref 0
  and j = ref (Array.length tab - 1)
  and s = ref 0
  in
    while !i < !j do
      s := (!i + !j) / 2;
      if tab.(!s) > x
      then i := !s + 1
      else j := !s
      done;
      !i
;;
(* dla tablicy malejącej; dla rosnącej < zamieniamy na > *)
let bin_search2 x tab =
  let i = ref 0
  and j = ref (Array.length tab - 1)
  and s = ref 0
  in
    while !i < !j do
      s := (!i + !j + 1) / 2;
      if tab.(!s) < x
      then j := !s - 1
      else i := !s
      done;
      !i
;;
let t1 = [|9; 4; 4; 2; -1; -1; -4; -32|];;
bin_search1 (-1) t1;;
bin_search1 4 t1;;
bin_search2 (-1) t1;;
bin_search2 4 t1;;

