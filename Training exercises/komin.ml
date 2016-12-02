let kominek k m x =
  let n = Array.length x in
    let wyniki = Array.make_matrix (k + 1) n (0, 0) in
      
      (* tablica posortowana malejąco *)
      Array.sort (fun a b -> compare b a) x;
      
      let oblicz j i =
        if i = n - 1 then
          (* sprawdzam, czy [n - 1]-szy kawałek drewna mieści się w kominku
             o pojemności [j] *)
          if x.(i) <= j
          (* dodaję [n - 1]-szy kawałek do kominka *)
          then wyniki.(j).(i) <- (x.(i), n - 1)
          (* nie dodaję [n - 1]-ego kawałka drewna, więc czas gaśnięcia jest
             zerowy, a największy kawałek włożony nie istnieje więc niech ma
             indeks [n] (poza listą) *)
          else wyniki.(j).(i) <- (0, n)
        else
          (* sprawdzam, czy [i]-ty kawałek drewna mieści się w kominku
             o pojemności [j] *)
          if x.(i) <= j
          then
            (* sprawdzam, czy lepiej go dodać czy nie, mając na uwadze, że
               temperatura nie może być większa niż [m] *)
            let t = min (fst wyniki.(j - x.(i)).(i + 1) + x.(i)) m in
              (* porównuję sumy czasu spalania i czasu gaśnięcia *)
              if fst wyniki.(j).(i + 1) + x.(snd wyniki.(j).(i + 1)) > t + x.(i)
              then wyniki.(j).(i) <- wyniki.(j).(i + 1)
              else wyniki.(j).(i) <- (t, i)
          else
            (* nie dodaję [i]-tego kawałka drewna *)
            wyniki.(j).(i) <- wyniki.(j).(i + 1)
      in
        for i = n - 1 downto 0 do
          for j = 0 to k do
            oblicz j i
          done
        done;
        fst wyniki.(k).(0) + x.(snd wyniki.(k).(0))
;;
assert (kominek 42 36 [|25; 15; 30|] = 61)
;;
assert (kominek 42 36 [|25; 30; 5|] = 65)
;;
assert (kominek 42 100 [|40; 7; 5; 5; 5; 5; 5; 5; 5|] = 80)
;;
assert (kominek 10 10 [|1; 2; 3|] = 9)
;;
assert (kominek 5 36 [|1; 4; 5|] = 10)
;;
