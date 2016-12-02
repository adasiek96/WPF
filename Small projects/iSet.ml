(* Zadanie 4: MODYFIKACJA DRZEW *)
(* Autor: Adam Sobecki, Recenzent: Marian Dziubiak *)
(* Rozwiązanie opiera się na bibliotece "PSet - Polymorphic sets", której
autorami są Xavier Leroy, Nicolas Cannasse i Markus Mottl, udostępnionej na
licencji GNU Lesser General Public License *)


(* OKREŚLENIE TYPU *)
(* [t]: [Empty] - drzewo puste lub [Node (l, (a, b), r, h, e)], gdzie [l] -
lewe poddrzewo, [r] - prawe poddrzewo, [h] - wysokość drzewa, [e] - liczba
elementów w obu poddrzewach (przez element rozumiemy liczbę całkowitą),
[(a, b)] - przedział od [a] do [b] znajdujący się w węźle *)
type t =
  | Node of t * (int * int) * t * int * int
  | Empty


(* FUNKCJE POMOCNICZE *)
(* [porownaj v1 v2] funkcja porównująca przedziały [v1] i [v2]
(sprawdzająca, czy przedziały są rozłączne bądź równe) *)
let porownaj (a1, a2) (b1, b2) = 
  if a2 < b1 then -1 else
    if b2 < a1 then 1 else 0

(* [wysokosc] zwraca wysokość drzewa *)
let wysokosc =
  function
    | Node (_, _, _, h, _) -> h
    | Empty -> 0

(* [liczba_elem] zwraca liczbę elementów w obu poddrzewach *)
let liczba_elem =
  function
    | Node (_, _, _, _, e) -> e
    | Empty -> 0

(* [l_elem_w] zwraca liczbę elementów w węźle *)
let l_elem_w =
  function
    | Node (_, (a, b), _, _, _) -> abs (b - a) + 1
    | Empty -> 0

(* [suma p q r s] zwraca sumę liczb [p], [q], [r] i [s], lub jeśli
przekracza ona [max_int] zwraca [max_int] *)
let suma p q r s =
  if max (max p q) (max r s) = max_int ||
    p >= max_int - q ||
    r >= max_int - s ||
    p + q >= max_int - r - s
  then max_int
  else p + q + r + s

(* [make l v r] tworzy drzewo o lewym poddrzewie [l], przedziale [v]
i prawym poddrzewie [r] *)
let make l v r =
  let e = suma (liczba_elem l) (l_elem_w l) (liczba_elem r) (l_elem_w r)
  in Node (l, v, r, max (wysokosc l) (wysokosc r) + 1, e)

(* [bal l v r] balansuje drzewo utworzone z lewego poddrzewa [l], wartości
w węźle [v] i prawego poddrzewa [r] tak, by maksymalna różnica wysokości
poddrzew nie była większa od 2. Zakładamy, że [l] jest zrównoważone
względem [r]. *)
let bal l v r =
  let hl = wysokosc l in
  let hr = wysokosc r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lv, lr, _, _) ->
        if wysokosc ll >= wysokosc lr then make ll lv (make lr v r)
        else
          (match lr with
          | Node (lrl, lrv, lrr, _, _) ->
              make (make ll lv lrl) lrv (make lrr v r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rv, rr, _, _) ->
        if wysokosc rr >= wysokosc rl then make (make l v rl) rv rr
        else
          (match rl with
          | Node (rll, rlv, rlr, _, _) ->
              make (make l v rll) rlv (make rlr rv rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l v r

(* [znajdz_lewy drzewo v] zwraca skrajnie lewy przedział - "sąsiedni"
z przedziałem [v], bądź mający z nim część wspólną lub zwraca wyjątek, gdy
taki przedział nie istnieje *)
(* Rozpatrzmy dwa przypadki (1) i (2):
(1) w przedziale znajdującym się w korzeniu drzewa znajduje się początek
przedziału [v] (lub przedział z korzenia jest tzw. "sąsiedni"), wtedy jest
on szukanym przez nas przedziałem
(2) początek przedziału [v] jest na lewo od początku przedziału
znajdującego się w korzeniu, wtedy szukamy początku przedziału z lewej
strony (jeśli nic nie znaleziono (zwrócono wyjątek), to jeśli przedziały
częściowo się pokrywają, to skrajnie lewy przedział spełniający tę własność
jest szukanym przedziałem); jeśli natomiast początek przedziału [v] jest
o więcej niż 1 w prawo od końca przedziału znajdującego się w korzeniu, to
szukamy na prawo *)
exception Brak_Skrajnego
let rec znajdz_lewy drzewo (a, b) = 
  match drzewo with
    | Empty -> raise Brak_Skrajnego
    | Node (l, (x, y), r, _, _) ->
      if (x <= a && a <= y) || (y = a - 1 && a != min_int) then (x, y) else
        if x > a then
          try znajdz_lewy l (a, b) with
            | Brak_Skrajnego ->
              if x <= b then (x, y)
              else raise Brak_Skrajnego
        else znajdz_lewy r (a, b)

(* [znajdz_prawy drzewo v] działa analogicznie do [znajdz_lewy drzewo v],
tylko że szuka po prawej stronie *)
let rec znajdz_prawy drzewo (a, b) = 
  match drzewo with
    | Empty -> raise Brak_Skrajnego
    | Node (l, (x, y), r, _, _) ->
      if (x <= b && b <= y) || (x = b + 1 && b != max_int) then (x, y) else
        if y < b then
          try znajdz_prawy r (a, b) with
            | Brak_Skrajnego ->
              if a <= y then (x, y)
              else raise Brak_Skrajnego
        else znajdz_prawy l (a, b) 

(* [znajdz_skrajne drzewo v] znajduje skrajnie lewy i prawy przedział dla
przedziału [v] i zwraca przedział będący sumą przedziału skrajnie lewego
i prawego, o ile w ogóle istnieją *)
let znajdz_skrajne drzewo v =
  let l' =
    try znajdz_lewy drzewo v with
      | Brak_Skrajnego -> v
  and r' =
    try znajdz_prawy drzewo v with
      | Brak_Skrajnego -> v
  in (l', r')

(* [min_elem drzewo] zwraca najmniejszy element z drzewa *)
let rec min_elem =
  function
    | Node (Empty, v, _, _, _) -> v
    | Node (l, _, _, _, _) -> min_elem l
    | Empty -> raise Not_found

(* [usun_min_elem drzewo] zwraca drzewo z wyłączeniem najmniejszego
elementu *)
let rec usun_min_elem =
  function
    | Node (Empty, _, r, _, _) -> r
    | Node (l, v, r, _, _) -> bal (usun_min_elem l) v r
    | Empty -> invalid_arg "ISet.usun_min_elem"

(* [add_one x drzewo] dodaje przedział [x] do drzewa zakładając, że [x]
jest rozłączny z wszystkimi przedziałami w zbiorze *)
let rec add_one x =
  function
    | Node (l, v, r, h, _) ->
      let c = porownaj x v in
        if c = 0 then make l x r else
          if c < 0 then
            let nl = add_one x l
            in bal nl v r
          else
            let nr = add_one x r
            in bal l v nr
    | Empty -> make Empty x Empty

(* [join l v r] zwraca drzewo zawierające elementy z drzew [l] i [r] oraz
przedział [v] *)
let rec join l v r =
  match (l, r) with
    | (Empty, _) -> add_one v r
    | (_, Empty) -> add_one v l
    | (Node (ll, lv, lr, lh, _), Node (rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else make l v r

(* [split_pset x drzewo] zwraca trójkę [(l, present, r)], gdzie [l] jest
drzewem zawierającym przedziały mniejsze od [x]; natomiast [r] - przedziały
większe od [x]; [present] - wartość logiczna oznaczająca istnienie [x]
w drzewie *)
let split_pset x drzewo =
  let rec loop x =
    function
      | Empty ->
        (Empty, false, Empty)
      | Node (l, v, r, _, _) ->
        let c = porownaj x v in
          if c = 0 then (l, true, r) else
            if c < 0 then
              let (ll, pres, rl) = loop x l
              in (ll, pres, join rl v r)
            else
              let (lr, pres, rr) = loop x r
              in (join l v lr, pres, rr)
  in loop x drzewo

(* [scal d1 d2] zwraca drzewo powstałe po połączeniu drzew [d1] oraz
[d2] w taki sposób, że w węźle nowo powstałego drzewa znajdzie się element
minimalny drzewa [d2] *)
let rec scal d1 d2 = 
  match d1, d2 with
    | Empty, _ -> d2
    | _, Empty -> d1
    | _ -> 
      let r = min_elem d2 in
        let d3 = usun_min_elem d2
        in join d1 r d3

(* [Nieprawidlowy_Przedzial] wyjątek zwracany, gdy wejściowy przedział jest
nieprawidłowy *)
exception Nieprawidlowy_Przedzial


(* FUNKCJE GŁÓWNE *)
(* [empty] - puste drzewo *)
let empty = Empty

(* [is_empty drzewo] zwraca [true] jeśli drzewo jest puste, w przeciwnym
razie zwraca [false] *)
let is_empty drzewo = drzewo = Empty

(* [add x drzewo] zwraca drzewo zawierające wszystkie przedziały z drzewa
[drzewo] oraz przedział [x] *)
(* Szukamy skrajnie lewego i prawego przedziału w drzewie dla przedziału
[x], a następnie za pomocą funkcji [split_pset] znajdujemy przedziały
mniejsze i większe od [x'], gdzie [x'] jest przedziałem utworzonym z [x]
oraz skrajnie lewego i prawego przedziału. Następnie tworzymy nowe drzewo
z przedziału [x'], przedziałów mniejszych od [x'] i przedziałów większych
od [x'] *)
let add x drzewo =
  if fst x > snd x then raise Nieprawidlowy_Przedzial else
    let (l', r') = znajdz_skrajne drzewo x in
      if l' = x && r' = x then add_one x drzewo else
        let (l_tree, _, r1_tree) = split_pset l' drzewo in
          let (_, _, r_tree) = split_pset r' drzewo in
            let x' = (min (fst l') (fst x), max (snd r') (snd x)) in
              join l_tree x' r_tree

(* [remove x drzewo] usuwa z drzewa przedział [x] i zwraca nowo powstałe
drzewo *)
(* Szukamy skrajnie lewego i prawego przedziału w drzewie dla przedziału
[x], a następnie za pomocą funkcji [split_pset] znajdujemy przedziały
mniejsze i większe od [x'], gdzie [x'] jest przedziałem utworzonym z [x]
oraz skrajnie lewego i prawego przedziału. Następnie łączymy je; dokładamy
do nich skrajnie lewy i prawy przedział bez części wspólnej z [x] *)
let remove x drzewo =
  if fst x > snd x then raise Nieprawidlowy_Przedzial else
    let (l', r') = znajdz_skrajne drzewo x in
      let (l_tree, _, r1_tree) = split_pset l' drzewo in
        let (_, _, r_tree) = split_pset r' r1_tree in
          let nowe_drzewo = scal l_tree r_tree in
            let nowe_drzewo2 =
              if fst l' < fst x
              then add (fst l', fst x - 1) nowe_drzewo
              else nowe_drzewo
            in
              if snd r' > snd x
              then add (snd x + 1, snd r') nowe_drzewo2
              else nowe_drzewo2

(* [mem n drzewo] sprawdza, czy liczba całkowita [n] należy do któregoś
z przedziałów w drzewie *)
let mem n drzewo =
  let rec loop =
    function
      | Node (l, (a, b), r, _, _) ->
        (a <= n && n <= b) || loop (if n < a then l else r)
      | Empty -> false
  in loop drzewo

(* [iter f drzewo] wykonuje funkcję [f] do każdego elementu z drzewa;
elementy te są przekazywane do [f] w kolejności rosnącej *)
let iter f drzewo =
  let rec loop =
    function
      | Empty -> ()
      | Node (l, k, r, _, _) ->
        loop l; f k; loop r
  in loop drzewo

(* [fold f drzewo] zwraca [(f xn ... (f x2 (f x1 a)) ... )], gdzie
x1, ..., xn są kolejnymi elementami z drzewa w kolejności rosnącej *)
let fold f drzewo acc =
  let rec loop acc =
    function
      | Empty -> acc
      | Node (l, k, r, _, _) ->
        loop (f k (loop acc l)) r
  in loop acc drzewo

(* [elements drzewo] zwraca listę elementów drzewa w kolejności rosnącej *)
let elements drzewo =
  let rec loop acc =
    function
      | Empty -> acc
      | Node (l, k, r, _, _) ->
        loop (k :: loop acc r) l
  in loop [] drzewo

(* [below n drzewo] zwraca liczbę elementów w drzewie mniejszych bądź
równych [n] *)
(* Przechodząc po drzewie kumulujemy wynik, jeśli dojdziemy do przedziału
zawierającego [n], to obliczamy nasz wynik i go zwracamy, w przeciwnym razie
dodajemy kolejne elementy mniejsze bądź równe [n] (dodajemy do naszego wyniku
ich liczebność) *)
let below n drzewo =
  let rec loop drzewo =
    match drzewo with
      | Node (l, v, r, _, _) ->
        let c = porownaj (n, n) v in
          if c = 0 then
            let vb = fst v
            in
              if n - vb + 1 <= 0 then max_int
              else suma (liczba_elem l) (l_elem_w l) (n - vb + 1) 0
          else
            if c < 0 then loop l
            else suma (liczba_elem l) (l_elem_w l) (l_elem_w drzewo) (loop r)
    | Empty -> 0
  in loop drzewo

(* [split n drzewo] zwraca trójkę [(l, present, r)], gdzie [l] jest
drzewem zawierającym przedziały mniejsze od [n]; natomiast [r] - przedziały
większe od [n]; [present] - wartość logiczna oznaczająca istnienie [n]
w jakimkolwiek przedziale znajdującym się w drzewie *)
(* Szukamy skrajnie lewego i prawego przedziału w drzewie dla przedziału
[(n, n)], a następnie za pomocą funkcji [split_pset] znajdujemy przedziały
mniejsze i większe od [(n, n)'], gdzie [(n, n)'] jest przedziałem
utworzonym z [(n, n)] oraz skrajnie lewego i prawego przedziału. Następnie
dodajemy elementy ze skrajnych przedziałów mniejsze/większe od [n] do drzew
[l] i [r]. Naszą trójkę uzupełniamy o wartość logiczną [present] *)
let split n drzewo =
  let (l', r') = znajdz_skrajne drzewo (n, n) in
    let (l_tree, _, r1_tree) = split_pset l' drzewo in
      let (_, _, r_tree) = split_pset r' r1_tree in
        let l_tree2 =
          if fst l' < n
          then add (fst l', n - 1) l_tree
          else l_tree
        and r_tree2 =
          if snd r' > n
          then add (n + 1, snd r') r_tree
          else r_tree
        in (l_tree2, mem n drzewo, r_tree2)
;;

(* TESTY )
let s = Node (Empty, (1, 25), Empty, 1, 0);;
assert (elements s = [(1, 25)]);;

let s = add (1, 40) s;;
assert (elements s = [(1, 40)]);;

let s = Node (Node (Empty, (1, 1), Empty, 1, 0), (4, 10), Empty, 2, 1);;
assert (elements s = [(1, 1); (4, 10)]);;

let s = add (2, 3) s;;
assert (elements s = [(1, 10)]);;

let s =
  Node
   (Node (Node (Empty, (1, 1), Empty, 1, 0), (3, 3),
     Node (Empty, (5, 5), Node (Empty, (7, 7), Empty, 1, 0), 2, 1), 3, 3),
   (9, 9),
   Node (Node (Empty, (11, 11), Empty, 1, 0), (13, 13),
    Node (Empty, (15, 15), Empty, 1, 0), 2, 2),
   4, 7);;
let t = [|0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8|];;
for i = 0 to 15 do assert (below i s = t.(i)); done;;

let s = add (2, 4) s;;
let t = [|0; 1; 2; 3; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10|];;
for i = 0 to 15 do assert (below i s = t.(i)); done;;

let s = empty;;
let s = add (10, max_int - 1) s;;
assert (below max_int s = 4611686018427387893);;

let s = add (1, 9) s;;
assert (below max_int s = 4611686018427387902);;

let s = add (max_int, max_int) s;;
assert (elements s = [(1, 4611686018427387903)]);;

let s = add (min_int, -1) s;;
assert (s =
  Node (Node (Empty, (-4611686018427387904, -1), Empty, 1, 0),
   (1, 4611686018427387903), Empty, 2, 4611686018427387903));;

let s = empty;;
let s = add (11, 11) (add (5, 6) (add (2, 3) (add (0, 0) (add (8,9)
  (add (13, 15) (add (17, 23) s))))));;

assert (remove (11, 11) s =
  Node
   (Node (Node (Empty, (0, 0), Empty, 1, 0), (2, 3),
     Node (Empty, (5, 6), Node (Empty, (8, 9), Empty, 1, 0), 2, 2), 3, 5),
   (13, 15), Node (Empty, (17, 23), Empty, 1, 0), 4, 14));;

assert (remove (2, 5) s =
  Node (Node (Empty, (0, 0), Node (Empty, (6, 6), Empty, 1, 0), 2, 1),
   (8, 9),
   Node (Node (Empty, (11, 11), Empty, 1, 0), (13, 15),
    Node (Empty, (17, 23), Empty, 1, 0), 2, 8),
   3, 13));;

assert (remove (17, 17) s =
  Node
   (Node (Node (Empty, (0, 0), Empty, 1, 0), (2, 3),
     Node (Empty, (5, 6), Empty, 1, 0), 2, 3),
   (8, 9),
   Node (Empty, (11, 11),
    Node (Empty, (13, 15), Node (Empty, (18, 23), Empty, 1, 0), 2, 6),
    3, 9), 4, 15));;

assert (remove (8, 10) s =
  Node
   (Node (Node (Empty, (0, 0), Empty, 1, 0), (2, 3),
     Node (Empty, (5, 6), Node (Empty, (11, 11), Empty, 1, 0), 2, 1),
     3, 4), (13, 15), Node (Empty, (17, 23), Empty, 1, 0), 4, 13));;

assert (remove (13, 15) s =
  Node (Node (Empty, (0, 0), Empty, 1, 0), (2, 3),
   Node (Node (Empty, (5, 6), Empty, 1, 0), (8, 9),
    Node (Empty, (11, 11), Node (Empty, (17, 23), Empty, 1, 0), 2, 7),
    3, 10), 4, 13));;

assert (remove (-10, 23) s = Empty);;

assert (split 0 s =
  (Empty, true,
   Node
    (Node (Node (Empty, (2, 3), Node (Empty, (5, 6), Empty, 1, 0), 2, 2),
      (8, 9), Node (Empty, (11, 11), Empty, 1, 0), 3, 5),
    (13, 15), Node (Empty, (17, 23), Empty, 1, 0), 4, 14)));;

assert (split 10 s =
  (Node (Node (Empty, (0, 0), Empty, 1, 0), (2, 3),
    Node (Empty, (5, 6), Node (Empty, (8, 9), Empty, 1, 0), 2, 2), 3, 5),
   false,
   Node (Node (Node (Empty, (11, 11), Empty, 1, 0), (13, 15), Empty, 2, 1),
    (17, 23), Empty, 3, 4)));;

let g = Node (Node (Node (Empty, (11, 11), Empty, 1, 0),
   (13, 15), Empty, 2, 1), (17, 23), Empty, 3, 4);;

assert (add (5, 5) g =
  Node (Node (Node (Empty, (5, 5), Empty, 1, 0), (11, 11), Empty, 2, 1),
   (13, 15), Node (Empty, (17, 23), Empty, 1, 0), 3, 9));;

assert (split 23 s =
  (Node
    (Node (Node (Empty, (0, 0), Empty, 1, 0), (2, 3),
      Node (Empty, (5, 6), Empty, 1, 0), 2, 3),
    (8, 9),
    Node (Empty, (11, 11),
     Node (Empty, (13, 15), Node (Empty, (17, 22), Empty, 1, 0), 2, 6),
     3, 9), 4, 15),
   true, Empty));;

assert (split 6 s =
  (Node (Empty, (0, 0),
    Node (Empty, (2, 3), Node (Empty, (5, 5), Empty, 1, 0), 2, 1), 3, 3),
   true,
   Node (Node (Empty, (8, 9), Node (Empty, (11, 11), Empty, 1, 0), 2, 1),
    (13, 15), Node (Empty, (17, 23), Empty, 1, 0), 3, 10)));;

assert (mem 14 s = true);;
assert (mem 7 s = false);;
assert (mem 17 s = true);;

assert (below 0 s = 1);;
assert (below 10 s = 7);;
assert (below 23 s = 18);;
( *)