let rev lista =
   List.fold_left (fun t h -> h :: t) [] lista
;;
let lista = [1; 2; 2; 3; 3; 4]
;;
rev lista
;;
open List
;;
let iloczyn_skalarny l1 l2 =
   fold_left2 (fun a x y -> a + x * y) 0 l1 l2
;;
let flatten1 lista =
   fold_right (@) lista []
;;
let flatten2 lista =
   fold_left (fun a l -> a @ l) [] lista
;;
flatten1 [[1; 2]; [5; 6]; [7; 8]]
;;
flatten2 [[1; 2]; [5; 6]; [7; 8]]
;;
let map f lista =
   fold_right (fun x a -> (f x) :: a) lista []
;;
let suma_wektorow l1 l2 =
   map2 (+) l1 l2
;;
let sito lista =
   match lista with
      | [] -> []
      | h :: t ->
         filter (function x -> x mod h <> 0) t
;;
let lista2 =
   [2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14;
      15; 16; 17; 18; 19; 20; 21; 22; 23]
;;
sito lista2
;;
let gen n =
   let rec pom n lista =
      if n = 1 then lista
      else pom (n - 1) (n :: lista)
   in pom n []
;;
let eratostenes n =
   let rec pom lista =
      if lista = [] then []
      else (List.hd lista) :: (pom (sito lista))
   in pom (gen n)
;;
eratostenes 42
;;

type 'a tree = Node of 'a tree * 'a * 'a tree | Null
;;
let rec fold_tree f a drzewo =
   match drzewo with
      | Null -> a
      | Node (l, v, p) ->
         f v (fold_tree f a l) (fold_tree f a p)
;;
let liczba_wezlow drzewo =
   fold_tree (fun _ a b -> a + b + 1) 0 drzewo
;;
let heigth drzewo =
   fold_tree (fun _ a b -> max a b + 1) 0 drzewo
;;
let srednica drzewo =
   fold_tree
      (fun _ (dL, wL) (dR, wR) ->
         (max dL dR + 1,
            max wL (max wR (dL + dR + 2))))
               (-1, -1) drzewo
;;
let infix drzewo =
   fold_tree
      (fun x fl fr y -> fl (x :: fr y)) (fun x -> x) drzewo []
;;
let tree =
   Node (Node (Node (Null, 7, Null), 5,
         Node (Null, 8, Null)), 4,
      Node (Node (Null, 9, Null), 6,
         Node (Null, 10, Null)))
;;
srednica tree
;;





