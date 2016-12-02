let segment tablica =
  let n = Array.length tablica
  and i = ref 0
  and j = ref 0
  and q = Queue.create ()
  and minimum = ref 0
  and licznik = ref 0 in
    Queue.add tablica.(0) q;
    while !j < n do
      minimum := Queue.fold min max_int q;
      if !minimum >= !j - !i + 1
        then begin
          licznik := max !licznik (!j - !i + 1);
          j := !j + 1;
          if !j < n then Queue.add tablica.(!j) q end
        else begin
          i := !i + 1;
          ignore (Queue.take q) end
    done;
    !licznik
;;
let tab = [|1; 3; 5; 3; 7; 2; 1; 6; 2; 9|];;
assert (segment tab = 3);;