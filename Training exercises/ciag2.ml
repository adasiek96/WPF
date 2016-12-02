let primes n = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43]
;;
let ciag n =
  let wyn = Array.make (n + 1) (0, 0)
  and mini = ref max_int
  and wynik = ref [n]
  and j = ref n in
    wyn.(1) <- (1, 1);
    for i = 2 to n do
      mini := List.fold_left (fun acc x ->
        if i mod x = 0 && fst wyn.(i / x) < acc
        then i / x else acc) !mini (primes n);
      if fst wyn.(!mini) > fst wyn.(i - 1)
      then mini := i - 1;
      wyn.(i) <- (fst wyn.(!mini) + 1, !mini);
      mini := max_int
    done;
    while !j != 1 do
      j := snd wyn.(!j);
      wynik := !j :: !wynik
    done;
    !wynik
;;
assert (ciag 42 = [1; 41; 42]);;
