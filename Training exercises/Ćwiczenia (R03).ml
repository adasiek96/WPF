
(*	Zadanie 4.A.	*)
(*let f x = x * (x - 2) * (x - 5) * (x - 10) * (x - 50);;
let p1 f n =
	let rec pom a b prefA prefB suma idx =
		if idx = n then (a, b) else
			if suma >= prefB then pom a idx prefA suma (suma + f (idx + 1)) (idx + 1) else
				if suma < prefA then pom idx b suma prefB (suma + f (idx + 1)) (idx + 1)
				else pom a b prefA prefB (suma + f (idx + 1)) (idx + 1)
	in pom 1 1 (f 1) (f 1) (f 1) 1;;
	
	
	
let f x = x * (x - 2) * (x - 5) * (x - 10) * (x - 50);;
let p1 f n =
	let rec pom a b prefA prefB sumaP idxP = funkcjaB b prefB sumaP idxP
	and funkcjaB b prefB suma idx =
		if idx = n then funkcjaA a prefA sumaP idxP else
			if suma >= prefB then funkcjaB idx suma (suma + f (idx + 1)) (idx + 1)
			else funkcjaB b prefB (suma + f (idx + 1)) (idx + 1)
	and funkcjaA a prefA suma idx =
		if idx = b then (a, b) else
			if suma < prefA then funkcjaA idx suma (suma + f (idx + 1)) (idx + 1)
			else funkcjaA a prefA (suma + f (idx + 1)) (idx + 1)
	in pom 1 1 (f 1) (f 1) (f 1) 1;;*)
	
(*
let f x = x * (x - 2) * (x - 5) * (x - 8) * (x - 10);;

let funB f n =
	let rec funkcjaB b prefB suma idx =
		if idx = n then b else
			if suma >= prefB then funkcjaB idx suma (suma + f (idx + 1)) (idx + 1) 
			else funkcjaB b prefB (suma + f (idx + 1)) (idx + 1)
	in funkcjaB 0 1 1 1;;
	
let funA f n =	
	let rec funkcjaA a prefA suma idx =
		if idx = n then a else
			if suma < prefA then funkcjaA idx suma (suma + f (idx + 1)) (idx + 1)
			else funkcjaA a prefA (suma + f (idx + 1)) (idx + 1)
	in funkcjaA 0 1 1 1;;
	
let p1 f n = (funA f (funB f n), funB f n);;
	
let f x = x * (x - 2) * (x - 5) * (x - 8) * (x - 10);;
let p f n =
		let rec funX minpref idxpref pref idx ress resl =
			if idx = n then true else
				if (minpref > pref + f idx) then 
					if (ress <= pref + f idx - minpref) then funX (pref + f idx) (idx + 1) (pref + f idx) (idx + 1) (pref + f idx - minpref) (idx - minpref + 1)
					else funX (pref + f idx) (idx + 1) (pref + f idx) (idx + 1) ress resl
				else
					if (ress <= pref - minpref) then funX minpref idxpref (pref + f idx) (idx + 1) (pref + f idx - minpref) (idx - idxpref + 1)
					else funX minpref idxpref (pref + f idx) (idx + 1) ress resl
		in funX 0 1 0 1 (f 1) 1;;
		
let p f n =
	let rec funX prefA a prefB b wynik =
		if b = n then true else
			if (prefA > prefB + f b) then
				if (suma <= prefB + f b - prefA) then
	
	
let p f n =
		let rec funX minpref idxpref pref idx ress =
			if idx = n then true else
				if (minpref <= pref + f idx) then
					if (ress <= pref + f idx - minpref) then funX minpref idxpref (pref + f idx) (idx + 1) (pref + f idx - minpref)
					else funX minpref idxpref (pref + f idx) (idx + 1) ress
				else
					if (ress <= pref - minpref) then (pref + f idx) (idx + 1) (pref + f idx) (idx + 1) (pref - minpref) (idx - minpref + 1)
					else funX (pref + f idx) (idx + 1) (pref + f idx) (idx + 1) ress
		in funX 0 1 0 1 (f 1) 1;;
	
	
	*)