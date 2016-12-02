type kolor = Czerwona | Zielona
;;
let kulki lista = 
  let (a, b, _) =
    List.fold_left (fun (r, g, i) x ->
      if x = Czerwona && i mod 2 = 0
      then (r + 1, g, i + 1) else
        if x = Zielona && i mod 2 = 1
        then (r, g + 1, i + 1)
        else (r, g, i + 1))
    (0, 0, 0) lista
  in min a b
;;
let lista =
  [Czerwona; Zielona; Zielona; Zielona; Zielona; Czerwona; Czerwona; Zielona]
;;
let lista1 =
  [Zielona; Czerwona; Czerwona; Czerwona; Zielona; Zielona; Czerwona; Zielona; Czerwona; Czerwona];;
assert (kulki lista = 2);;
