type graph = int list array;;
let path g =
  let n = Array.length g in
    let tab = Array.make n 0
    and m = ref 0 in
      for i = n - 1 downto 0 do
        tab.(i) <- List.fold_left (fun a j ->
          if j > i then max (tab.(j) + 1) a else a)
        tab.(i) (g.(i));
        m := max tab.(i) !m
      done;
      !m
;;
let graf =
  [|[5]; [2]; [1; 4; 5]; [5]; [2]; [0; 2; 3; 8; 10]; [10]; [10]; [5]; [10];
    [5; 6; 7; 9]|];;
assert (path graf = 3);;
