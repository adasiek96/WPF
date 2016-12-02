let rec silnia1 x =
	if x < 2 then 1
	else x * silnia1 (x - 1);;
	
let silnia2 x =
	let rec sil a x =
		if x < 2 then a
		else sil (a * x) (x - 1)
	in sil 1 x;;

let rec fibonacci1 x =
	if x < 2 then x
	else fibonacci1 (x - 1) + fibonacci1 (x - 2);;

let fibonacci2 x =
	let rec fib a b x =
		if x = 0 then a
		else fib b (a + b) (x - 1)
	in fib 0 1 x;;

let zad1 x =
	let rec z1 a x =
		if x mod 2 = 1 then a
		else z1 (a + 1) (x / 2)
	in if x = 0 then -1 else z1 0 x;;
	
let zad2 x =
	let rec z2 a x =
		if x = 0 then a
		else z2 (10 * a + x mod 10) (x / 10)
	in z2 0 x;;

let zad3a x =
	let rec z3 i s x =
		if x < s then i - 1
		else z3 (i + 1) (s + i + i + 1) x
	in z3 1 1 x;;
	
let zad3b x =
	let rec z3 i x =
		if x < i * i then i - 1
		else z3 (i + 1) x
	in z3 1 x;;
	
let zad4 x =
	let rec z4 i a x =
		if i > a then true else
			if x mod i = 0 then false
			else z4 (i + 1) a x
	in if x = 1 then false else z4 2 (zad3a x) x;;
	
let rec czyrzadka x =
	if x = 0 then true else
		if x mod 2 = 1 && (x / 2) mod 2 = 1 then false
		else czyrzadka (x / 2);;
	
let rec zad5 x =
	if czyrzadka (x + 1) = true then x + 1
	else zad5 (x + 1);;
	
let zad6 x =
	let rec z6 a x =
		if x = 0 then a else
			if czyrzadka x = true then z6 (a + 1) (x - 1)
			else z6 a (x - 1)
	in z6 0 x;;

let suma x =
	let rec s a x =
		if x = 0 then a
		else s (a + x mod 10) (x / 10)
	in s 0 x;;
	
let rec zad7 x =
	if x < 10 then
		if x = 9 || x = 0 then true
		else false
	else zad7 (suma x);;

let rec rsum x =
	let rec rs i a x =
		if x = 0 then a else
			if i mod 2 = 0 then rs (i + 1) (a + x mod 10) (x / 10)
			else rs (i + 1) (a - x mod 10) (x / 10)
	in rs 0 0 x;;

let rec zad8 x =
	if x < 11 then
		if x = 0 then true
		else false
	else zad8 (rsum x);;

let zad9a x y =
	let rec sumaxy a i x y =
		if x = 0 && y = 0 then (10 * a + i)
		else sumaxy ((10 * a + x mod 10) * 10 + y mod 10) (i + 1) (x / 10) (y / 10)
	in sumaxy 0 0 x y;;

let zad9b x =
	let rec liczbax a i x = 
		if i = 0 then a
		else liczbax (10 * a + ((x / 10) mod 10)) (i - 1) (x / 100)
	in liczbax 0 (x mod 10) (x / 10);;
	
let zad9c y =
	let rec liczbay a i y = 
		if i = 0 then a
		else liczbay (10 * a + y mod 10) (i - 1) (y / 100)
	in liczbay 0 (y mod 10) (y / 10);;

let zad10 n =
	let rec funkcja i n =
		if i = n - 2 then false else
			if i * i mod n = 1 then true
			else funkcja (i + 1) n
	in if n <= 3 then false else funkcja 2 n;;
	