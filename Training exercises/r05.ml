let liczba_roz_elem1 lista =
   let rec pom lista n =
      match lista with
         | [] -> n
         | h1 :: h2 :: t ->
            if h1 = h2 then pom (h2 :: t) n
            else pom (h2 :: t) (n + 1)
         | h :: t -> n + 1
   in pom lista 0
;;
let liczba_roz_elem2 lista =
   let rec pom lista n =
      match lista with
         | [] -> n
         | h1 :: t1 ->
            match t1 with
               | [] -> n + 1
               | h2 :: t2 ->
                  if h1 = h2 then pom t1 n
                  else pom t1 (n + 1)
   in pom lista 0
;;
let lista1 =
   [1; 1; 2; 2; 2; 3; 4; 4; 5; 6; 7; 7];;
liczba_roz_elem1 lista1;;
liczba_roz_elem2 lista1;;
let lre lista = liczba_roz_elem1 lista;;
let a1 = [1; 2; 2; 3];;let a2 = [5; 6; 6; 7];;
let b1 = [3; 4; 4; 5];;let b2 = [4; 4; 5]
;;

let licznosc a b =
   let rec pom a b n d =
      match a, b with
         | h1 :: t1, h2 :: t2 ->
            if h1 > h2 then pom b a n d
            else
               if h1 = d then pom t1 b n d
               else pom t1 b (n + 1) h1
         | [], h2 :: t2 ->
            if h2 = d then pom [] t2 n d
            else n + lre b
         | [], [] -> n
   in
      match a, b with
         | [], [] -> 0
         | [], b -> lre b
         | a, [] -> lre a
         | h1 :: t1, h2 :: t2 ->
            if h1 > h2 then pom a t2 1 h2
            else pom t1 b 1 h1
;;
licznosc a1 a2;;licznosc a1 b1;;licznosc a1 b2;;
licznosc a2 b1;;licznosc a2 b2;;licznosc b1 b2;;
licznosc a2 a1;;licznosc b1 a1;;licznosc b2 a1;;
licznosc b1 a2;;licznosc b2 a2;;licznosc b2 b1;;

let id = function x -> x
;;
let compose f g = function x -> f (g x)
;;
let rec iter1 n f =
   if n = 0 then id
   else compose (iter1 (n - 1) f) f
;;
let rec iter0 n f x =
   if n = 0 then x
   else iter0 (n - 1) f (f x)
;;
let rec iter0 n f =
   if n = 0 then id
   else iter0 (n - 1) (compose f id)
;;

let wygladz1 f dx x =
   (f (x -. dx) +. f x +. f (x +. dx)) /. 3.
;;
let wygladz2 dx =
   fun f x ->
      (f (x -. dx) +. f x +. f (x +. dx)) /. 3.
;;






