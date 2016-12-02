let plecak k x =
  let n = Array.length x in
  let wynik = Array.make_matrix (n + 1) (k + 1) 0 in
    
    for i = 0 to n do
      wynik.(i).(0) <- 0
    done;
    for j = 0 to k do
      wynik.(0).(j) <- 0
    done;
    
    for i = 1 to n do
      for j = 0 to k do
        if x.(i - 1) > j
        then wynik.(i).(j) <- wynik.(i - 1).(j)
        else wynik.(i).(j) <- max wynik.(i - 1).(j) (wynik.(i - 1).(j - x.(i - 1)) + x.(i - 1))
      done
    done;
    
    wynik
;;
plecak 15 [|3; 5; 8; 6; 4; 2|]
;;