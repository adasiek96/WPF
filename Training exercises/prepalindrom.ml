let czy_palindrom a i j =
  let lewo = ref i
  and prawo = ref j
  and b = ref true in
    while !lewo < !prawo do
      if compare a.(!lewo) a.(!prawo) != 0 then b := false;
      lewo := !lewo + 1;
      prawo := !prawo - 1
    done;
    !b
;;
let prepalindrom a =
  let n = Array.length a
  and licznik = ref 0 in
    for j = 0 to n - 1 do
      if czy_palindrom a 0 j
        then licznik := max !licznik (j + 1)
    done;
    !licznik
;;
let tab1 = [|1; 2; 1; 1; 2; 1; 1; 2; 1|];;
let tab2 = [|"a"; "d"; "a"; "m"|];;
let tab3 = [|1; 2; 1; 3|];;
assert (prepalindrom tab1 = 9);;
assert (prepalindrom tab2 = 3);;
assert (prepalindrom tab3 = 3);;
