let kominek k m tab =
  let n = Array.length tab
  and czas li = List.fold_left max 0 li
  and temp li = min m (List.fold_left (+) 0 li)
  and uzyty = Array.to_list d in
    let rec find g temp czas li maximum =
      let 
  