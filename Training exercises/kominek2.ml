(* Zadanie 2 egzamin *)

let kominek k m d =
  let n = Array.length d in
  let wyniki = Array.make_matrix k n (0, 0) in
  let x = Array.copy d in
  Array.sort (fun x y-> compare y x) x;
  let find (w:int) i =
    if i = n-1 then
      if x.(i) > w then wyniki.(w).(i) <- (n+1, 0) 
      else wyniki.(w).(i) <- (i, x.(i))
    else
        if w<x.(i) then wyniki.(w).(i) <- wyniki.(w).(i+1)
        else
          let (mni, mnw) = wyniki.(w-x.(i)).(i+1) in
          let mnw = mnw +x.(i) in
          let (mxi, mxw) = wyniki.(w).(i+1) in
          let mnw = min m mnw in
          let (mi, mw) = if mnw > mxw then (mni, mnw) else (mxi, mxw) in
          wyniki.(w).(i) <- (mi, mw)
  in for i = n-1 downto 0 do
    for j = 0 to k-1 do
      find j i;
      print_int i; print_int j
    done;
  done;
  snd wyniki.(k-1).(0)
          
let _ = assert(kominek 42 36 [|25;15;30|] = 61)
