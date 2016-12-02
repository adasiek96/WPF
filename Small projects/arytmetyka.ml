(*Author: Adam Sobecki*)
(*Code review: Aliaksei Suvorau*)
(* Określenie typu *)
(*-----------------*)

type wartosc = Zakres of float * float;;


(*	Funkcje pomocnicze *)
(*--------------------*)

(* funkcje wybierajace element, jeśli możliwe rozny od nan, taki ktory jest minimalny/maksymalny *)
let minX a b =
	if compare a nan = 0 then b else
	if compare b nan = 0 then a else
	if a <= b then a else b;;

let maxX a b =
	if compare a nan = 0 then b else
	if compare b nan = 0 then a else
	if a >= b then a else b;;

(* funkcje liczace minimum/maksimum z 4 liczb *)
let minX4 a b c d =
	minX (minX a b) (minX c d);; 

let maxX4 a b c d =
	maxX (maxX a b) (maxX c d);;

(* funkcje wykonujace konkretne dzialanie dla 3 liczb *)
let dzialanie_min a b c f =	
	minX (f a b) (f a c);;
	
let dzialanie_max a b c f =	
	maxX (f a b) (f a c);;

(* przedzial [a;b] taki że a > b jest traktowany jako
suma przedzialow [-inf,b] i [a,inf] *)
let przedzial_zwykly w =
	let Zakres(a, b) = w
	in a <= b;;

let in_wartosc w x =
	let Zakres(a, b) = w
	in
		if przedzial_zwykly w
		then a <= x && x <= b
		else x >= a || x <= b;;		

(* wartosc dzialania [1;1] / [a,b] dla przedzialu zwyklego *)
let odwrotnosc w =
	let Zakres(a, b) = w
	in
		if in_wartosc w 0. then
			if a = 0. && b = 0. then Zakres(infinity, infinity) else
			if a = 0. then Zakres(1. /. b, infinity) else
			if b = 0. then Zakres(neg_infinity, 1. /. a)
			else Zakres(1. /. b, 1. /. a)
		else Zakres(minX (1. /. a) (1. /. b), maxX (1. /. a) (1. /. b));;

(* wartosc dzialania [1;1] / [a,b] dla przedzialu nie zwyklego *)
let odwrotnosc2 w =
	let Zakres(a, b) = w
	in
		if a *. b > 0. then Zakres(neg_infinity, infinity)
		else
			if a = 0. then Zakres(1. /. b, infinity) else
			if b = 0. then Zakres(neg_infinity, 1. /. a)
				else Zakres(1. /. b, 1. /. a);;


(* Konstruktory *)
(*--------------*)

let wartosc_dokladnosc x p =
	let a = x -. x *. p /. 100.
	and b = x +. x *. p /. 100.
	in Zakres(minX a b, maxX a b);;
		
let wartosc_od_do x y =
	Zakres(x, y);;
	
let wartosc_dokladna x =
	Zakres(x, x);;

let in_wartosc w x =
	let Zakres(a, b) = w
	in
		if przedzial_zwykly w
		then a <= x && x <= b
		else x >= a || x <= b;;

(* Selektory *)
(*-----------*)

let in_wartosc w x =
	let Zakres(a, b) = w
	in
		if przedzial_zwykly w
		then a <= x && x <= b
		else x >= a || x <= b;;

let min_wartosc w =
	let Zakres(a, b) = w
	in 
		if przedzial_zwykly w then a
		else neg_infinity;;

let max_wartosc w =
	let Zakres(a, b) = w
	in
		if przedzial_zwykly w then b
		else infinity;;	

let sr_wartosc w =
	if przedzial_zwykly w
	then (min_wartosc w +. max_wartosc w) /. 2.
	else nan;;


(* Modyfikatory *)
(*--------------*)

let plus w1 w2 =
	if not(przedzial_zwykly w1) && not(przedzial_zwykly w2)
	then Zakres(neg_infinity, infinity) else
		let Zakres(a1, b1) = w1
		and Zakres(a2, b2) = w2
		in
			if not(przedzial_zwykly w1) || not(przedzial_zwykly w2)
			then
				if a1 +. a2 <= b1 +. b2
				then Zakres(neg_infinity, infinity)
				else Zakres(a1 +. a2, b1 +. b2)
			else Zakres(a1 +. a2, b1 +. b2);;

let minus w1 w2 =
	if not(przedzial_zwykly w1) && not(przedzial_zwykly w2)
	then Zakres(neg_infinity, infinity) else
		let Zakres(a1, b1) = w1
		and Zakres(a2, b2) = w2
		in
			if not(przedzial_zwykly w1) || not(przedzial_zwykly w2)
			then
				if a1 -. b2 <= b1 -. a2
				then Zakres(neg_infinity, infinity)
				else Zakres(a1 -. b2, b1 -. a2)
			else Zakres(a1 -. b2, b1 -. a2);;

let rec razy w1 w2 =
	if not(przedzial_zwykly w1) && not(przedzial_zwykly w2)
	then Zakres(neg_infinity, infinity) else
		let Zakres(a1, b1) = w1
		and Zakres(a2, b2) = w2
		in
			if not(przedzial_zwykly w1) || not(przedzial_zwykly w2) then
				if przedzial_zwykly w2 then
					if a2 = 0. && b2 = 0. then Zakres(nan, nan) else
					if a2 *. b2 <= 0. then Zakres(neg_infinity, infinity) else
					if b2 < 0. then
						if dzialanie_min b1 a2 b2 ( *. ) <= dzialanie_max a1 a2 b2 ( *. )
						then Zakres(neg_infinity, infinity)
						else Zakres(dzialanie_min b1 a2 b2 ( *. ),
							dzialanie_max a1 a2 b2 ( *. ))
					else 
						if dzialanie_min a1 a2 b2 ( *. ) <= dzialanie_max b1 a2 b2 ( *. )
						then Zakres(neg_infinity, infinity)
						else Zakres(dzialanie_min a1 a2 b2 ( *. ),
							dzialanie_max b1 a2 b2 ( *. ))
				else
						razy w2 w1
			else Zakres(minX4 (a1 *. a2) (b1 *. b2) (a1 *. b2) (b1 *. a2),
				maxX4 (a1 *. a2) (b1 *. b2) (a1 *. b2) (b1 *. a2));;

let podzielic w1 w2 =
	let Zakres(a1, b1) = w1
	and Zakres(a2, b2) = w2
	in
		if a2 = 0. && b2 = 0. && a1 *. b1 < 0. then Zakres(infinity, neg_infinity)
		else
			if not(przedzial_zwykly w1)
			then Zakres(neg_infinity, infinity) else
				if not(przedzial_zwykly w2) then razy w1 (odwrotnosc2 w2)
				else razy w1 (odwrotnosc w2);;


(* TESTY *)
let w1 = wartosc_dokladnosc 50. 5.;;
let w2 = wartosc_dokladnosc (-13.) 50.;;

let w3 = wartosc_od_do 0. 6.;;
let w4 = wartosc_od_do (-2.) 3.;;

let w5 = wartosc_dokladna 5.;;
let w6 = wartosc_dokladna 0.;;

assert (min_wartosc (plus w1 w3) = 47.5);;
assert (max_wartosc (plus w4 w6) = 3.);;

assert (min_wartosc (minus w1 w5) = 42.5);;
assert (max_wartosc (minus w6 w2) = 19.5);;

assert (min_wartosc (razy w2 w4) = -58.5);;
assert (max_wartosc (razy w3 w5) = 30.);;

assert (min_wartosc (podzielic w3 w6) = infinity);;
assert (max_wartosc (podzielic w2 w5) = -1.3);;

assert (plus (Zakres (-5., 2.)) (Zakres (0., 6.)) = Zakres(-5., 8.));;
assert (razy (Zakres (-1., -5.)) (Zakres (-2., -1.)) = Zakres(5., 2.));;
assert (podzielic (Zakres (0., 0.)) (Zakres (5., 0.)) = Zakres(0., 0.));;
assert (podzielic (Zakres (0., 2.)) (Zakres (4., 4.)) = Zakres(0., 0.5));;
assert (razy (Zakres (-1., -5.)) (Zakres (-2., 6.)) = Zakres(neg_infinity, infinity));;
assert (razy (Zakres (5., -5.)) (Zakres (2., 6.)) = Zakres(10., -10.));;
assert (razy (Zakres (5., -5.)) (Zakres (-2., 6.)) = Zakres(neg_infinity, infinity));;
assert (razy (Zakres (5., -5.)) (Zakres (-6., -2.)) = Zakres(10., -10.));;
assert (podzielic (Zakres (-6., -2.)) (Zakres (-5., 2.)) = Zakres(0.4, -1.));;
assert (podzielic (Zakres (-3., 2.)) (Zakres (0., 0.)) = Zakres(infinity, neg_infinity));;
assert (podzielic (Zakres (1., 5.)) (Zakres (-5., 0.)) = Zakres(neg_infinity, -0.2));;
assert (podzielic (Zakres (0., 1.)) (Zakres (0., 0.)) = Zakres(infinity, infinity));;
assert (podzielic (Zakres (-2., 3.)) (Zakres (0., 0.)) = Zakres(infinity, neg_infinity));;
assert (razy (Zakres (0.5, -0.2)) (Zakres (-6., -2.)) = Zakres(0.4, -1.));;
assert (podzielic (Zakres (-2., 1.)) (Zakres (0., 0.)) = Zakres(infinity, neg_infinity));;
