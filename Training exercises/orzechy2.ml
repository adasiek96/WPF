(* *)
let dzielniki k =
  let lista = ref [] in
    for i = 1 to k do
      if k mod i = 0
      then lista := (i, k / i) :: !lista
    done;
    !lista
;;
let pref tab m n =
  let tab_pref = Array.make_matrix m n 0 in
    for i = 0 to m - 1 do
      tab_pref.(i).(0) <- tab.(i).(0);
      for j = 1 to n - 1 do
        tab_pref.(i).(j) <- tab.(i).(j) + tab_pref.(i).(j - 1)
      done
    done;
    tab_pref
;;
let pref2 tab m n =
  let tab_pref2 = Array.make_matrix m n 0 in
    for j = 0 to n - 1 do
      tab_pref2.(0).(j) <- tab.(0).(j);
      for i = 1 to m - 1 do
        tab_pref2.(i).(j) <- tab.(i).(j) + tab_pref2.(i - 1).(j)
      done
    done;
    tab_pref2
;;
let sprawdz (a, b) tab_pref tab_pref2 m n =
  let suma = ref 0
  and suma_max = ref 0 in
    if b <= n then
      for i = 0 to m - a do
        for k = i to a - 1 + i do
          suma := !suma + tab_pref.(k).(b - 1)
        done;
        for j = 0 to n - b - 1 do
          if i != 0 then
          suma := max !suma (- tab_pref.(i + a - 1).(j) + tab_pref.(i + a - 1).(j + b)
          + tab_pref.(i - 1).(j) - tab_pref.(i - 1).(j + b))
          else suma := max !suma (- tab_pref.(i + a - 1).(j) + tab_pref.(i + a - 1).(j + b))
        done;
        suma_max := max !suma_max !suma;
        suma := 0
      done;
      !suma_max
;;
let orzechy k tab =
  let m = Array.length tab
  and n = Array.length tab.(0) in
    List.fold_left (fun acc (a, b) ->
      max acc (sprawdz (a, b) (pref tab m n) (pref2 tab m n) m n))
    0 (dzielniki k)
;;
let tab =
  [|[|0; 0; 4; 1; 2; 0|]; [|3; 3; 8; 11; 3; 2|]; [|1; 3; 9; 8; 1; 2|];
    [|2; 1; 1; 2; 0; 12|]|];;
let k = 6;;
orzechy k tab;;
