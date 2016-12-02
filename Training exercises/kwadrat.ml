(* funkcja licząca sumy prefiksowe dla każdego wiersza *)
let sumy tab m n =
  let suma_pref = Array.make_matrix m n 0 in
  let licznik = ref 0 in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      if tab.(i).(j) then
        begin licznik := !licznik + 1;
        suma_pref.(i).(j) <- !licznik end
      else
        begin licznik := 0;
        suma_pref.(i).(j) <- !licznik end
    done
  done;
  suma_pref
;;
exception Stop
;;
let binar suma_pref m n j wyn =
  let b = ref false
  and licznik = ref 0
  and d = ref 0
  and wynik = ref wyn
  and mini = ref j
  and maxi = ref (n - 1) in
        (* środek przedziału [(mini, maxi)] *)
        d := (!maxi + !mini) / 2;
        (try while !mini <= !d && !d <= !maxi do
          for i = 0 to m - 1 do
            (* warunek, aby dany wiersz mógł być fragmentem kwadratu *)
            if suma_pref.(i).(!d) - suma_pref.(i).(!mini) = !d - !mini
              && suma_pref.(i).(!mini) != 0
            then begin
              (* ilość wierszy pod rząd, które mogłyby być fragmentami
                 kwadratu *)
              licznik := !licznik + 1;
              (* jeśli poniższy warunek spełniony, znaleźliśmy kwadrat *)
              if !licznik = !d - !mini + 1 then b := true end
            else licznik := 0
          done;
          if !b then begin
            wynik := max !wynik (!d - !mini + 1);
            b := false;
            (* rozszerzamy przedział poszukiwań *)
            if !d = !maxi then raise Stop;
            d := (!maxi + (!d + 1)) / 2 end
          else begin
            b := false;
            (* skracamy przedział poszukiwań *)
            if !d = !maxi then raise Stop;
            maxi := !d;
            d := (!d + !mini) / 2 end
        done with Stop -> ());
        !wynik
;;
let kwadrat tab =
  let m = Array.length tab
  and n = Array.length tab.(0) in
    let suma_pref = sumy tab m n
    and wyn = ref 0 in
      for j = 0 to n - 2 do
        wyn := max !wyn (binar suma_pref m n j !wyn)
      done;
      !wyn
;;

let tab1 =
  [|
    [|false; false; false|];
    [|false; true; false|];
    [|true; true; true|];
    [|false; true; false|]
  |]
;;
let tab2 =
  [|
    [|false; false; false|];
    [|false; true; false|];
    [|false; true; true|];
    [|false; true; false|]
  |]
;;
let tab3 =
  [|
    [|false; false; false|];
    [|false; true; true|];
    [|false; true; true|];
    [|false; false; false|]
  |]
;;
let tab4 =
  [|
    [|false; false; false|];
    [|false; false; false|];
    [|true; false; false|];
    [|false; false; false|]
  |]
;;
