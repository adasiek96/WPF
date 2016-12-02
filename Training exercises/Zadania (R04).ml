(*	Zadanie 1.	*)
let lista_n_liczb n =
	let rec lnl n l =
		if n = 0 then l
		else lnl (n - 1) (n :: l)
	in lnl n [];;


(*	Zadanie 2.	*)
let dublowanie x =
	let rec dub x y =
		match x with
			| [] -> y
			| h :: t -> dub t (h :: h :: y)
	in dub (List.rev x) [];;

	
(*	Zadanie 3.	*)
let rec last l =
	match l with
		| h :: [] -> h
		| h :: t -> last t;;

		
(*	Zadanie 4.	*)
let head l n =
	let rec pom l k n =
		if n = 0 then k else
			match l with
				| [] -> k
				| h::t -> pom t (h :: k) (n - 1)
	in List.rev (pom l [] n);;

let tail l n =
	let rec pom l k n =
		if n = 0 then k else
			match l with
				| [] -> k
				| h::t -> pom t (h :: k) (n - 1)
	in pom (List.rev l) [] n;;


(*	Zadanie 5.	*)
let shuffle x y =
	let rec shf x y z =
		match x, y with
			| [], [] -> z
			| h1 :: t1, h2 :: t2 -> shf t1 t2 (h2 :: h1 :: z)
			| h :: t, [] -> shf t y (h :: z)
			| [], h :: t -> shf x t (h :: z)
	in List.rev (shf x y []);;

	
(*	Zadanie 6.	*)
let lp2nl lp =
	let rec funA lp k =
		match lp with
			| [] -> k
			| (a, b) :: t -> funA t (funB k a b)
	and funB l n x =
		if n = 0 then l
		else funB (x :: l) (n - 1) x
	in funA (List.rev lp) [];;


(*	Zadanie 7.	*)
let tails l =
	let rec pom l k =
		match l with
			| [] -> k
			| _ :: t -> pom t (t :: k)
	in List.rev (pom l [l]);;


(*	Zadanie 11.A.	*)
let dominujacy lista_p =
	let rec dom a n lista =
		match lista with
			| [] ->
				if n > 1 then (true, a) else
					if n = 0 then (false, 0)
					else sprawdz a 0 lista_p
			| h :: t ->
				if h = a then dom a (n + 1) t else
					if n = 0 then dom h 1 t
					else dom a (n - 1) t
	and sprawdz a i lista =
			match lista with
			| [] ->
				if i > 0 then (true, a)
				else (false, 0)
			| h :: t ->
				if h = a then sprawdz a (i + 1) t
				else sprawdz a (i - 1) t
	in
		match lista_p with
			| [] -> (false, 0)
			| h :: t -> dom h 1 t;;
	
	
(*	Zadanie 11.B.	*)
let dominujacy lista_p =
	let rec sprawdz a i lista_p =
		match lista_p with
			| [] ->
				if i > 0 then (true, a)
				else (false, 0)
			| h :: t ->
				if h = a then sprawdz a (i + 1) t
				else sprawdz a (i - 1) t
	in
		let rec dom a n lista =
			match lista with
				| [] ->
					if n > 1 then (true, a) else
						if n = 0 then (false, 0)
						else sprawdz a 0 lista_p
				| h :: t ->
					if h = a then dom a (n + 1) t else
						if n = 0 then dom h 1 t
						else dom a (n - 1) t
		in
			match lista_p with
				| [] -> (false, 0)
				| h :: t -> dom h 1 t;;

(*	Zadanie 15.	*)
let trojki l =
	let rec funA l k =
		match l with
			| [] -> k
			| h :: t -> funA t (funB t k h)
	and funB l k p1 =
		match l with
			| [] -> k
			| h :: t -> funB t (funC t k p1 h) p1
	and funC l k p1 p2 =
		match l with
			| [] -> k
			| h :: t ->
				if p1 + p2 > h then funC t ((p1, p2, h) :: k) p1 p2
				else k
	in funA l [];;




				
	