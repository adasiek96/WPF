(*	Zadanie 2.A.	*)
let potegowanie1 x y =
	let rec pot1 a y =
		if y = 0 then a
		else pot1 (a * x) (y - 1)
	in pot1 1 y;;


(*	Zadanie 2.B.	*)
let potegowanie2 x y =
	let rec pot2 a x y =
		if y = 0 then a else
			if y = 0 mod 2 then pot2 a (x * x) (y / 2)
			else pot2 (a * x) x (y - 1)
	in pot2 1 x y;;

(*	Zadanie 3. (błędne)	*)
let f x = - 1 * x * (x - 2) * (x - 5) * (x - 7) * (x - 10);;
let p f n =
		let rec funX minpref idxpref pref idx ress a b =
			if idx = n then (a, b) else
				if minpref > pref + f idx then
					if ress <= pref - minpref then funX (pref + f idx) (idx + 1) (pref + f idx) (idx + 1) (pref - minpref) (idxpref + 1) (idx - 1)
					else funX (pref + f idx) (idx + 1) (pref + f idx) (idx + 1) ress a b
				else
					if ress <= pref + f idx then funX minpref idxpref (pref + f idx) (idx + 1) (pref + f idx) idxpref idx
					else funX minpref idxpref (pref + f idx) (idx + 1) ress a b
		in funX 0 1 0 1 (f 1) 1 1;;
		
(*	Zadanie 3. (chyba poprawne)	*)
let f x = - 1 * x * (x - 2) * (x - 5) * (x - 7) * (x - 10);;
let p f n =
		let rec funX minpref idxpref pref idx ress a b =
			if idx > n then (a, b) else
				if minpref > pref + f idx then funX (pref + f idx) (idx + 1) (pref + f idx) (idx + 1) ress a b
				else
					if ress <= pref + f idx then funX minpref idxpref (pref + f idx) (idx + 1) (pref + f idx) idxpref idx
					else funX minpref idxpref (pref + f idx) (idx + 1) ress a b
		in funX 0 1 0 1 (f 1) 1 1;;
		
		
	