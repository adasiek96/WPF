

let ciag n =
  let f = [|1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233|]
  and m = ref max_int
  and tab = Array.make (n + 1) (0,[]) in

      for i = 1 to n do
        m := max_int;
        for j = 0 to Array.length f - 1 do
          if i mod f.(j) = 0
          then m := min !m (tab.(i / f.(j)))
        done;
        if !m > tab.(i - 1) then
          tab.(i) <- (tab.(i - 1) + 1, )
        else
          tab.(i) <- (tab.(i / !m)  1, )
      done;
      tab.(n) - 1
;;
let ciag n =
  let f = [|1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233|]
  and m = ref max_int
  (* (długość ścieżki, ścieżka) *)
  and tab = Array.make (n + 1) (0, []) in
      for i = 1 to n do
        m := max_int;
        for j = 0 to Array.length f - 1 do
          if i mod f.(j) = 0 then
            begin
              if fst tab.(i) <= fst tab.(i / f.(j)) + 1
              then tab.(i) <- (fst tab.(i / f.(j)) + 1, (f.(j)) :: (snd tab.(i)))
            end;
         if fst tab.(i) < fst tab.(i - 1) + 1
           then tab.(i) <- (fst tab.(i - 1) + 1, i :: (snd tab.(i)))
        done; 
      done;
      List.rev (snd tab.(n))
;;