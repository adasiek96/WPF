let silnie n =
  let wynik = ref []
  and silnia = ref 1
  and k = ref 1
  and suma = ref n in
    while !silnia < !suma do
      k := !k + 1;
      silnia := !silnia * !k
    done;
    while !k > 0 do
      while !silnia <= !suma do
        suma :=  !suma - !silnia;
        wynik := !k :: !wynik
      done;
      silnia := !silnia / !k;
      k := !k - 1
    done;
    !wynik
;;
let n = 42;;
assert (silnie 42 = [3; 3; 3; 4]);;
