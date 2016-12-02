(* znajduje najmniejszy i największy element tablicy o wymiarach [m] na [n] *)
let znajdz tab n m =
  let minimum = ref max_int
  and maximum = ref 0 in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        minimum := min !minimum tab.(i).(j);
        maximum := max !maximum tab.(i).(j)
      done
    done;
    (!minimum, !maximum)
;;
(* stan odwiedzenia wierzchołka (pola tablicy) *)
type stan = Odwiedzony | Nieodwiedzony
;;
(* zwraca listę sąsiadów dla zadanego wierzchołka *)
let sasiad (i, j) n m =
  List.fold_left (fun a (x, y) ->
    if i + x <= n - 1 && i + x >= 0 && j + y <= m - 1 && j + y >= 0
    then (i + x, j + y) :: a else a)
  [] [(1, 0); (0, 1); (-1, 0); (0, -1)]
;;
(* funkcja pomocnicza do dfs-a, która odwiedza sąsiadów wierzchołka *)
let rec odwiedz (i, j) tab k_tab n m wys =
  if tab.(i).(j) <= wys then begin
    k_tab.(i).(j) <- Odwiedzony;
    List.iter (fun (x, y) ->
      if k_tab.(x).(y) = Nieodwiedzony
      then odwiedz (x, y) tab k_tab n m wys)
    (sasiad (i, j) n m) end
;;
(* zwraca [true], gdy można przejść z pola [(0, 0)] do [(n - 1, m - 1)] przy
   wysokości [wys] *)
let dfs tab n m wys =
  let k_tab = Array.make_matrix n m Nieodwiedzony in
    odwiedz (0, 0) tab k_tab n m wys;
    if k_tab.(n - 1).(m - 1) = Odwiedzony then true else false
;;
let wysokosc tab =
  let n = Array.length tab
  and m = Array.length tab.(0) in
    let minimum = ref (fst (znajdz tab n m))
    and maximum = ref (snd (znajdz tab n m))
    and wys = ref 0 in
      while !minimum < !maximum do
        wys := (!minimum + !maximum) / 2;
        if dfs tab n m !wys
        then maximum := !wys
        else minimum := !wys + 1
      done;
      !maximum
;;
let dane1 =
  [|[|1; 2; 5|]; [|2; 3; 5|]; [|2; 4; 3|]|];;
let dane2 =
  [|[|1; 8; 3; 1; 10|]; [|1; 3; 4; 2; 10|]; [|1; 2; 5; 9; 2|];
  [|2; 3; 4; 9; 1|]; [|5; 4; 6; 8; 1|]; [|1; 2; 7; 2; 1|]|];;
assert (wysokosc dane1 = 4);;
assert (wysokosc dane2 = 7);;
