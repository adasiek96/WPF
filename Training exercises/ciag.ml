let rec fibonacci n =
  let rec pom a b lista =
    if a > n then lista
    else pom b (a + b) (a :: lista)
  in pom 2 3 []
;;
(* [wybierz f i tab] - wybiera tylko te liczby fibonacciego, które dzielą [i],
   wybiera minimum po pierwszej współrzędnej pary [(x, y)] = [tab.(i / fib)],
   dla wszystkich [fib], następnie zwraca parę [(x + 1, i / fib)] taką,
   że [x] jest najmniejszą liczbą wyrazów ciągu, a [i / fib] to poprzedni
   wyraz ciągu *)
let wybierz f i tab =
  let lista = List.filter (fun x -> i mod x = 0) f in
    List.fold_left (fun acc fib ->
      if fst tab.(i / fib) < fst acc
      then (fst tab.(i / fib) + 1, i / fib) else acc)
    (max_int, 0) lista
;;
let ciag n =
  if n = 0 then [] else
    (* (a, b) - (długość ciągu, indeks ostatniego wyrazu ciągu) *)
    let tab = Array.make (n + 1) (0, 0)
    and f = List.rev (fibonacci n) in
      tab.(1) <- (1, 1);
      for i = 2 to n do
        let (a, b) = wybierz f i tab in
        let (c, d) = (fst tab.(i - 1) + 1, i - 1) in
          if a < c then tab.(i) <- (a, b) else tab.(i) <- (c, d)
      done;
      let wynik = ref [n]
      and i = ref n in
        while !i != 1 do
          i := snd tab.(!i);
          wynik := !i :: !wynik
        done;
        !wynik
;;
assert (ciag 23 = [1; 21; 22; 23]);;
assert (ciag 42 = [1; 2; 42]);;
assert (ciag 33 = [1; 3; 4; 32; 33]);;
assert (ciag 17 = [1; 2; 16; 17]);;
assert (ciag 8 = [1; 8]);;
assert (ciag 0 = []);;
assert (ciag 1 = [1]);;
assert (ciag 2 = [1; 2]);;
assert (ciag 3 = [1; 3]);;
assert (ciag 4 = [1; 3; 4]);;
assert (ciag 44 = [1; 21; 22; 44]);;