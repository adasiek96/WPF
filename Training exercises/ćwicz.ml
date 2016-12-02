type 'a drzewo = N of 'a * ('a drzewo list)
;;
let postfixPL drzewo =
   let rec pom1 (N (v, lista)) a =
      match lista with
         | [] -> v :: a
         | _ -> pom2 lista (v :: a)
   and pom2 lista a =
      match lista with
         | [] -> a
         | h :: t -> pom2 t (pom1 h a)
   in pom1 drzewo []
;;
let postfixLP drzewo =
   let rec pom1 (N (v, lista)) a =
      match lista with
         | [] -> v :: a
         | _ -> pom2 lista (v :: a)
   and pom2 lista a =
      match lista with
         | [] -> a
         | h :: t -> pom1 h (pom2 t a)
   in pom1 drzewo []
;;
let prefixPL =
   List.rev (postfixLP)
;;
let prefixLP =
   List.rev (postfixPL)
;;
let prefixPL drzewo =
   let rec pom1 (N (v, lista)) a =
      match lista with
         | [] -> v :: a
         | _ -> v :: (pom2 lista a)
   and pom2 lista a =
      match lista with
         | [] -> a
         | h :: t -> pom2 t (pom1 h a)
   in pom1 drzewo []
;;
let prefixLP drzewo =
   let rec pom1 (N (v, lista)) a =
      match lista with
         | [] -> v :: a
         | _ -> v :: (pom2 lista a)
   and pom2 lista a =
      match lista with
         | [] -> a
         | h :: t -> pom1 h (pom2 t a)
   in pom1 drzewo []
;;
let drzewo =
   N (4, [N (5, [N (7, []); N (8, [])]);
      N (6, [N (9, []); N (10, [])])])
;;
prefixLP drzewo
;;
prefixPL drzewo
;;
let glebokosc drzewo =
   let rec pom1 (N (v, lista)) =
      match lista with
         | [] -> 1
         | _ -> pom2 lista 1
   and pom2 lista n =
      match lista with
         | [] -> n
         | h :: t ->
            pom2 t (max n ((pom1 h) + 1))
   in pom1 drzewo
;;
glebokosc drzewo
;;
let liczba_elementow drzewo =
   let rec pom1 (N (v, lista)) =
      match lista with
         | [] -> 1
         | _ -> pom2 lista 1
   and pom2 lista n =
      match lista with
         | [] -> n
         | h :: t -> pom2 t ((pom1 h) + n)
   in pom1 drzewo
;;
liczba_elementow drzewo
;;

type 'a tree =
   Node of 'a tree * 'a * 'a tree | Null
;;
let infixPL_bin tree =
   let rec pom tree a =
      match tree with
         | Null -> a
         | Node (l, v, p) ->
            pom p (v :: (pom l a))
   in pom tree []
;;
let infixLP_bin tree =
   let rec pom tree a =
      match tree with
         | Null -> a
         | Node (l, v, p) ->
            pom l (v :: (pom p a))
   in pom tree []
;;
let prefixLP_bin tree =
   let rec pom tree a =
      match tree with
         | Null -> a
         | Node (l, v, p) ->
            v :: (pom l (pom p a))
   in pom tree []
;;
let prefixPL_bin tree =
   let rec pom tree a =
      match tree with
         | Null -> a
         | Node (l, v, p) ->
            v :: (pom p (pom l a))
   in pom tree []
;;
let postfixPL_bin tree =
   let rec pom tree a =
      match tree with
         | Null -> a
         | Node (l, v, p) ->
            (pom p (pom l (v :: a)))
   in pom tree []
;;
let postfixLP_bin tree =
   let rec pom tree a =
      match tree with
         | Null -> a
         | Node (l, v, p) ->
            (pom l (pom p (v :: a)))
   in pom tree []
;;
let tree =
   Node (Node (Node (Null, 7, Null), 5,
         Node (Null, 8, Null)), 4,
      Node (Node (Null, 9, Null), 6,
         Node(Null, 10, Null)))
;;
infixLP_bin tree
;;
infixPL_bin tree
;;
prefixPL_bin tree
;;
prefixLP_bin tree
;;
postfixPL_bin tree
;;
postfixLP_bin tree
;;
let liczba_elem tree =
   let rec pom tree n =
      match tree with
         | Null -> n
         | Node (l, v, p) ->
            pom l ((pom p 1) + n)
   in pom tree 0
;;
liczba_elem tree
;;
let lista_elem tree =
   let rec pom tree a =
      match tree with
         | Null -> a
         | Node (l, v, p) ->
            match a with
               | [] ->
                  1 :: pom l (pom p [])
               | h :: t ->
                  (h + 1) :: pom l (pom p t)
   in pom tree []
;;
lista_elem tree
;;

type drzewo =
   | Node of drzewo * drzewo
   | Leaf
;;
let lista_wierzcholkow drzewo =
   let rec pom drzewo a =
      match drzewo with
         | Leaf -> a
         | Node (l, p) ->
            match a with
               | [] ->
                  1 :: (pom l (pom p []))
               | h :: t ->
                  (h + 1) :: (pom l (pom p t))
   in pom drzewo []
;;
let drzewo =
   Node
     (Node (Leaf, Node (Leaf, Leaf)),
      Node (Node (Leaf, Leaf),
         Node (Node (Leaf, Leaf), Leaf)))
;;
lista_wierzcholkow drzewo
;;








