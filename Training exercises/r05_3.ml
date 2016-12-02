type 'a tree = N of 'a * 'a tree list
;;
let rec fold_tree f (N (v, lista)) =
   f v (map (fold_tree f) lista)
;;
let height drzewo =
   let maxl l = fold_left max 0 l
   in
      fold_tree (fun _ l -> maxl l + 1) drzewo
;;
let height2 drzewo =
   fold_tree
      (fun _ l -> fold_left max 0 l + 1) drzewo
;;
let liczba_wezlow drzewo =
   fold_tree
      (fun _ l -> fold_left (+) 0 l + 1) drzewo
;;
let drzewo =
   N (4, [N (5, [N (7, []); N (8, [])]);
      N (6, [N (9, []); N (10, [])])])
;;
liczba_wezlow drzewo
;;
let srednica drzewo =
   fold_tree
      (fun _ l -> fold_left (max) 0 l + 1) drzewo....
;;
