(*	Ćwiczenie 1. (silnia)	*)
let rec silnia x =
	if x <= 1 then 1
	else x * silnia (x - 1);;

(*	Ćwiczenie 2. (Fibonacci)	*)
let rec fibonacci x =
	if x < 2 then x
	else fibonacci (x - 1) + fibonacci (x - 2);;

(*	Ćwiczenie 3. (silnia_A)	*)
let silnia x =
	let rec sil x a =
		if x <= 1 then a
		else sil (x - 1) (a * x)
	in sil x 1;;

(*	Ćwiczenie 4. (Fibonacci_A)	*)
let fibonacci x =
	let rec fib a b x =
		if x = 0 then a
		else fib b (a + b) (x - 1)
	in fib 0 1 x;;

(*	Zadanie 1.I.	*)
let parzystosc x =
	let rec sp x a =
		if x = 0 then -1 else
			if x mod 2 = 1 then a
			else sp (x / 2) (a + 1)
	in sp x 0;;
	
(*	Zadanie 1.II.	*)
let parzystosc x =
	let rec sp x a =
		if x mod 2 = 1 then a
		else sp (x / 2) (a + 1)
	in if x = 0 then -1 else sp x 0;;
	
(*	Zadanie 2.	*)
let odwrotnie x =
	let rec odwr x y =
		if x = 0 then y
		else odwr (x / 10) (10 * y + x mod 10)
	in odwr x 0;;

(*	Zadanie 3.	*)
let sqrt x =
	let rec pod x s i =
		if x < s then i - 1
		else pod x (s + 2 * i + 1) (i + 1)
	in pod x 0 0;;
						
(*	Zadanie 10. *)
let issqrt x =
	let rec pom x i =
		if x - 1 = i then false else
			if i * i mod x = 1 then true
			else pom x (i + 1)
	in if x < 3 then false else pom x 2;;