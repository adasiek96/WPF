(* ORIGAMI *)
(* Autor: Adam Sobecki; *)
(* Recenzent: Filip Tokarski (grupa dr Mirosławy Miłkowskiej) *)


(* DEKLARACJA TYPÓW *)
(* Punkt na płaszczyźnie *)
type point = float * float

(* Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym
punkcie *)
type kartka = point -> int


(* FUNKCJE POMOCNICZE *)
open List
let e = 1. /. 10000000000.

(* [x (===) y] zwraca [true] jeśli x = y z dokładnością do [e], w przeciwnym
razie zwraca [false] *)
let (===) =
  (fun x y -> x < y +. e && x > y -. e)

(* [x (<==) y] zwraca [true] jeśli x = y z dokładnością do [e] lub x < y
w przeciwnym razie zwraca [false] *)
let (<==) =
  (fun x y -> (x < y +. e && x > y -. e) || x < y)

(* [kwadrat x] zwraca kwadrat liczby zmiennoprzecinkowej [x] *)
let kwadrat x =
   x *. x

(* [kw_odleglosci p1 p2] zwraca kwadrat odległości pomiędzy punktami [p1]
oraz [p2] *)
let kw_odleglosci (x1, y1) (x2, y2) =
   kwadrat (x2 -. x1) +. kwadrat (y2 -. y1)

(* [punkt_sym p1 p2 p] zwraca punkt symetryczny do punktu p względej
prostej wyznaczonej przez punkty [p1] [p2]. Szukamy współczynników [a1],
[b1] prostej przechodzącej przez punkty [p1] i [p2] oraz współczynników
[a2], [b2] prostej do niej prostopadłej przechodzącej przez punkt [p].
Następnie wyznaczamy punkt przecięcia tych prostych, a na końcu szukamy
punktu symetrycznego do punktu [p] *)
let punkt_sym (x1, y1) (x2, y2) (x, y) =
   if y1 === y2 then (x, 2. *. y1 -. y) else
      if x1 === x2 then (2. *. x1 -. x, y) else
         let a1 = (y2 -. y1) /. (x2 -. x1)
         and a2 = (x1 -. x2) /. (y2 -. y1) in
            let b1 = y2 -. a1 *. x2
            and b2 = y -. a2 *. x in
               let p = (b2 -. b1) /. (a1 -. a2) in
                  let q = a1 *. p +. b1 in
                     (2. *. p -. x, 2. *. q -. y)

(* [iloczyn_wektorowy p1 p2 p3] zwraca współrzędną [z] wektora otrzymanego
z iloczynu wektorowego wektorów [p1p2] i [p1p3]. Znak mówi o położeniu punktu
[p3] względej prostej wyznaczonej przez [p1] i [p2]. Gdy jest on dodatni, punkt
znajduje się po lewej stronie prostej, a jeśli ujemny, to po prawej *)
let iloczyn_wektorowy (x1, y1) (x2, y2) (x3, y3) =
   (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1)

(* [po_prawej p1 p2 p] sprawdza czy punkt [p] znajduje się po prawej
stronie zgięcia *)
let po_prawej (x1, y1) (x2, y2) (x, y) =
   iloczyn_wektorowy (x1, y1) (x2, y2) (x, y) < 0.


(* FUNKCJE GŁÓWNE *)
(* [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół od
punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz (lub na
krawędziach) prostokąta, kartka zostanie przebita 1 raz, w pozostałych
przypadkach 0 razy *)
let prostokat (x1, y1) (x2, y2) =
   fun (x, y) ->
      if x1 <== x && x <== x2 && y1 <== y && y <== y2
      then 1 else 0

(* [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku
w punkcie [p] i promieniu [r] *)
let kolko (x1, y1) r =
   fun (x, y) ->
      if kw_odleglosci (x1, y1) (x, y) <== kwadrat r
      then 1 else 0

(* [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany w ten
sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
przebicie po prawej stronie prostej powinno więc zwrócić 0. Przebicie
dokładnie na prostej powinno zwrócić tyle samo, co przebicie kartki przed
złożeniem. Po stronie lewej - tyle co przed złożeniem plus przebicie
rozłożonej kartki w punkcie, który nałożył się na punkt przebicia. *)
let zloz (x1, y1) (x2, y2) kartka =
   fun (x, y) ->
      if po_prawej (x1, y1) (x2, y2) (x, y) then 0 else
         let (x', y') = punkt_sym (x1, y1) (x2, y2) (x, y) in
            if x' === x && y' === y then kartka (x, y)
            else kartka (x, y) + kartka (x', y')

(* [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ...
(zloz p1_1 p2_1 k)...)] czyli wynikiem jest złożenie kartki [k] kolejno
wzdłuż wszystkich prostych z listy *)
let skladaj listapkt kartka =
   let f k (p1, p2) =
      zloz p1 p2 k
   in List.fold_left f kartka listapkt
;;

(* TESTY )
let prosto1 = prostokat (1., 0.) (6., 4.);;
assert (prosto1 (3., 4.) = 1);;
assert (prosto1 (0., 0.) = 0);;

let p1 = (5., -2.);;
let p2 = (2., 4.);;
let p3 = (3.1, 0.5);;
assert (fst (punkt_sym p1 p2 (punkt_sym p1 p2 p3)) === 3.1);;
assert (snd (punkt_sym p1 p2 (punkt_sym p1 p2 p3)) === 0.5);;
assert (fst (punkt_sym p2 p1 (punkt_sym p1 p2 p3)) === 3.1);;
assert (snd (punkt_sym p2 p1 (punkt_sym p1 p2 p3)) === 0.5);;

let p4 = (-2., -2.);;
assert (zloz p1 p2 prosto1 p3 = 2);;

let lista1 = [(p1, p2); ((3., 4.), (3., 0.))];;
let lista2 = [((0., -4.), (0., 4.)); ((4., 0.), (-4., 0.))];;
let prosto2 = prostokat (-3., -3.) (3., 3.);;
assert (skladaj lista1 prosto1 p3 = 4);;
assert (skladaj lista2 prosto2 p4 = 4);;

assert (punkt_sym (0., 4.) (0., -4.) (-2., -2.) = (2., -2.));;

assert (po_prawej p4 (0., 4.) (0., -4.));;
assert (po_prawej p4 (-4., 0.) (4., 0.));;

let p5 = (2., 2.);;
let kol = kolko (3., 2.) (2.);;
assert (kol p5 = 1);;
assert (zloz (4., 0.) p2 kol p5 = 2);;

let prosto3 = prostokat (0., 0.) (4., 1.);;
assert (prosto3 (0.000001, 0.) = 1);;

assert (zloz (1., 0.) (1., 1.) prosto3 (0., 0.) = 2);;
assert (zloz (1., 0.) (1., 1.) prosto3 (-1., 0.) = 1);;
assert (zloz (1., 0.) (1., 1.) prosto3 (2., 0.) = 0);;

let lista3 = [(1., 0.), (1., 1.); (0., 0.), (0., 1.)];;
assert (skladaj lista3 prosto3 (0.0001, 0.1) = 0);;
assert (skladaj lista3 prosto3 (0., 0.5) = 2);;
assert (skladaj lista3 prosto3 (0.1, 1.) = 0);;
assert (skladaj lista3 prosto3 (-0.6, 0.5) = 3);;
assert (skladaj lista3 prosto3 (-0.9999, 1.) = 3);;
assert (skladaj lista3 prosto3 (-0.9999, -0.0001) = 0);;
assert (skladaj lista3 prosto3 (-1.0, 1.) = 2);;
assert (skladaj lista3 prosto3 (-1.0001, 1.) = 1);;
assert (skladaj lista3 prosto3 (-2.0000, 0.5) = 1);;
assert (skladaj lista3 prosto3 (-2.0001, 1.) = 0);;

let lista4 = [(1., 0.), (1., 1.); (0., 0.), (0., 1.);(-0.6, 0.), (-0.6, 1.)];;
assert (skladaj lista4 prosto3 (0., 0.5) = 0);;
assert (skladaj lista4 prosto3 (-0.6, 0.5) = 3);;
assert (skladaj lista4 prosto3 (-0.7, 0.5) = 6);;
assert (skladaj lista4 prosto3 (-1., 0.5) = 5);;
assert (skladaj lista4 prosto3 (-1.02, 0.5) = 4);;
assert (skladaj lista4 prosto3 (-1.2, 0.5) = 3);;
assert (skladaj lista4 prosto3 (-1.3, 0.5) = 1);;
( *)
