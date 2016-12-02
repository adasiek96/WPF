(* Zadanie 2 egzamin *)

let kominek k m x =
  let n = Array.length x in
    let wyniki = Array.make_matrix k n (0, 0) in
      Array.sort (fun a b -> compare b a) x;
      let find w i =
        if i = n - 1 then
          if w < x.(i) then wyniki.(w).(i) <- (n + 1, 0) 
          else wyniki.(w).(i) <- (i, x.(i))
        else
          if w < x.(i) then wyniki.(w).(i) <- wyniki.(w).(i + 1)
          else
            let (mni, mnw) = wyniki.(w - x.(i)).(i + 1) in
              let mnw = mnw + x.(i) in
                let (mxi, mxw) = wyniki.(w).(i + 1) in
                  let mnw = min m mnw in
                    let (mi, mw) =
                      if mnw > mxw then (mni, mnw) else (mxi, mxw) in
                        wyniki.(w).(i) <- (mi, mw)
      in
        for i = n - 1 downto 0 do
          for j = 0 to k - 1 do
            find j i;
          done;
        done;
        snd wyniki.(k-1).(0)
;;
let kominek k m x =
  let n = Array.length x in
    let wyniki = Array.make k (0, n) in
      Array.sort (fun a b -> compare b a) x;
      for i = 0 to k - 1 do
        for j = 0 to n - 1 do
          if x.(j) <= i
          then wyniki.(i) <- max wyniki.(i) (min (fst wyniki.(i - x.(j)) + x.(j)) m, x.(j))
        done
      done;
      fst wyniki.(k - 1) + snd wyniki.(k - 1)
;;
kominek 42 36 [|25; 15; 30|]
;;
kominek 42 36 [|25; 30; 5|]
;;
(* nie działa *)
(* Zadanie 2 egzamin *)

let kominek k m x =
  let n = Array.length x in
    let wyniki = Array.make_matrix k n (0, 0) in
      Array.sort (fun a b -> compare b a) x;
      let find w i =
        if i = n - 1 then
          if w < x.(i) then wyniki.(w).(i) <- (n + 1, 0) 
          else wyniki.(w).(i) <- (i, x.(i))
        else
          if w < x.(i) then wyniki.(w).(i) <- wyniki.(w).(i + 1)
          else
            let (mni, mnw) = wyniki.(w - x.(i)).(i + 1) in
              let mnw = mnw + x.(i) in
                let (mxi, mxw) = wyniki.(w).(i + 1) in
                  let mnw = min m mnw in
                    let (mi, mw) =
                      if mnw > mxw then (mni, mnw) else (mxi, mxw) in
                        wyniki.(w).(i) <- (mi, mw)
      in
        for i = n - 1 downto 0 do
          for j = 0 to k - 1 do
            find j i;
          done;
        done;
        snd wyniki.(k-1).(0)
;;
let kominek k m x =
  let n = Array.length x in
    let wyniki = Array.make k (0, n) in
      Array.sort (fun a b -> compare b a) x;
      for i = 0 to k - 1 do
        for j = 0 to n - 1 do
          if x.(j) <= i
          then wyniki.(i) <- max wyniki.(i) (min (fst wyniki.(i - x.(j)) + x.(j)) m, x.(j))
        done
      done;
      fst wyniki.(k - 1) + snd wyniki.(k - 1)
;;
kominek 42 36 [|25; 15; 30|]
;;
kominek 42 36 [|25; 30; 5|]
;;
(* nie działa *)

let kominek k m x =
  let n = Array.length x in
  let wyniki = Array.make_matrix (n + 1) (k + 1) 0
  and r = ref (n - 1) in  
    (* przygotowanie tablicy wyników *)
    for i = 0 to n do (* gdy pojemność jest równa 0 *)
      wyniki.(i).(0) <- 0
    done;
    for j = 0 to k do (* gdy mamy n niewykorzystanych drewienek *)
      wyniki.(n).(j) <- 0
    done;
    
    (* tablica posortowana malejąco *)
    Array.sort (fun a b -> compare b a) x;
    
    (* znajdujemy element największy x_r *)
    for t = 0 to n - 2 do
      r := t;
      if x.(!r) <= k then
      (* rozważenie kolejno i przedmiotów począwszy od x_(n-1) aż do x_(r+1) *)
      for i = n - 1 downto !r + 1 do
        for j = 0 to k - x.(!r) do
          if x.(i) <= j (* czy i-ty element mieści się w plecaku o pojemności j *)
          then wyniki.(i).(j) <- max wyniki.(i + 1).(j) (min (wyniki.(i + 1).(j - x.(i)) + x.(i)) m)
          else wyniki.(i).(j) <- wyniki.(i + 1).(j)
        done
      done
    done;
    wyniki
;;
kominek 42 36 [|25; 15; 30|]
;;
kominek 42 36 [|25; 30; 5|]
;;


