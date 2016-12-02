(* [sufit x y] - zwraca sufit ilorazu [x] przez [y] *)
let sufit x y =
  if x mod y = 0 then x / y
  else x / y + 1
;;
(* [Stop] - wyjątek, gdy chcemy użyć więcej podpór niż posiadamy *)
exception Stop
;;
(* [czy_mozliwe tab c d k n] - sprawdza, czy przy zadanym [d] jesteśmy
    w stanie tak ustawić podpory, by most się nie zawalił *)
let czy_mozliwe tab c d k n =
  let lewo = ref 0
  and prawo = ref 1
  and mini = ref tab.(0)
  and licznik = ref 0
  and b = ref true in
    (try while !prawo <= n - 1 do
      mini := min !mini tab.(!prawo);
      if !mini < (sufit (!prawo - !lewo + 1) d) * c
      then begin
        if !licznik = k then raise Stop
        else licznik := !licznik + 1;
        lewo := !prawo;
        prawo := !prawo + 1;
        mini := tab.(!lewo)
      end else prawo := !prawo + 1
    done with Stop -> b := false);
    !b
;;
let most tab k =
  let n = Array.length tab
  and c = Array.fold_left min max_int tab in
    let left = ref 0
    and right = ref (n - 1)
    and middle = ref 0 in
      while !left < !right do
        middle := (!left + !right) / 2;
        if czy_mozliwe tab c !middle k n
        then right := !middle
        else left := !middle + 1
      done;
      !right
;;
let tab = [|5; 10; 7; 6; 9; 10; 10; 11; 12; 10; 6; 8|];;
let k = 3;;
assert (most tab k = 3);;
