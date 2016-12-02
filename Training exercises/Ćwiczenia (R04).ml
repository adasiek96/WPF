let rec lp2pl l =
	match l with
		| [] -> ([], [])
		| (a, b) :: t ->
			let (l1, l2) = pom t
			in (a :: l1) (b :: l2)

let lp2pl l =
	let rec pom l l1 l2 =
		match l with
			| [] -> (l1, l2)
			| (a, b) :: t ->
            pom t (a :: l1) (b :: l2)
	in pom (List.rev l) [] [];;


let rec pl2lp paralist =
	match paralist with
		| ([], []) -> []
		| (h1 :: t1, h2 :: t2) ->
			let listapar = pl2lp (t1, t2)
			in ((h1, h2) :: listapar);;

let pl2lp paralist =
	let rec pom paralist listapar =
		match paralist with
			| ([], []) -> listapar
			| (h1 :: t1, h2 :: t2) ->
            pom ((t1, t2)) ((h1, h2) :: listapar)
	in List.rev (pom paralist []);;

	
let trojki l =
	let rec funA l k =
		match l with
			| [] -> k
			| h :: t -> funA t (funB t k h)
	and funB l k p1 =
		match l with
			| [] -> k
			| h :: t -> funB t (funC t k p1 h) h
	and funC l k p1 p2 =
		match l with
			| [] -> k
			| h :: t ->			
				if (p1 + p2 > h)
            then funC t ((p1, p2, h) :: k) p1 p2
				else k
	in funA l [];;
	
type 'a drzewo = N of 'a * (('a drzewo) list);;
let d1 = N (3, []);;
let d2 = N (4, [N (5, [N (7, []); N (8, [])]); N (6, [N (9, []); N (10, [])])]);;

let postfix tr =
	let rec pom (N (w, l)) a =
		match l with
			| [] -> w :: a
			| _ -> plist l (w :: a)
	and plist l a =
		match l with
			| [] -> a
			| h :: t -> plist t (pom h a)
	in pom tr [];;	

type tree = Node of tree * int * tree | Null;;

let symetryczne tr =
	let rec spr t1 t2 =
		match t1, t2 with
			| Null, Null -> true
			| Node (x1, w1, y1), Node (x2, w2, y2) ->
				if w1 = w2 then spr x1 x2 && spr y1 y2
				else false
			| _ -> false
	in
		match tr with
			| Null -> true
			| Node (t1, _, t2) -> spr t1 t2;;

type tr = Node of tr * tr | Leaf;;

type 'a tree = N of 'a * ('a tree) list;;

let rec glebokosc (N (_, lista)) =
	match lista with
		| [] -> 1
		| _ -> mierz lista 0
and mierz l a =
	match l with
		| [] -> a
		| h :: t -> mierz t (max a ((glebokosc h) + 1));;

let rec liczba_elementow (N (_, lista)) =
	match lista with
		| [] -> 1
		| _ -> pom lista 1
and pom lista a =
	match lista with
		| [] -> a
		| h :: t -> pom t (a + liczba_elementow h);;

let postfixPL drzewo =
	let rec pom (N (v, lista)) a =
		match lista with
			| [] -> v :: a
			| _ -> plist lista (v :: a)
	and plist lista a =
		match lista with
			| [] -> a
			| h :: t -> plist t (pom h a)
	in pom drzewo [];;

let prefiksPL drzewo =
	let rec pom (N (v, lista)) a =
		match lista with
			| [] -> v :: a
			| _ -> plist lista (v :: a)
	and plist lista a =
		match lista with
			| [] -> a
			| h :: t -> pom h (plist t a)
	in List.rev (pom drzewo []);;
			
let drzewo =
(N (1, 
	[N (2, 
		[N (3,
			[N (4,
				[N (5,
					[N (6, [])]);
					N (0, [])])])]);
				
	N (7,
		[N (8,
			[N (9, [])])]);
	N (10,
		[N (11,
			[N (12,
				[N (13,
					[N (14,
						[N (15, []);
						N (16, [])])])])])])]));;			
let drzewo2 = (N (1, [N (2, [N (3, [N (4, [N (5, [N (6, [])])])])]);
	N (7, [N (8, [N (9, [])])]); N (10, [N (11, [N (12, [N (13, [N (14, [N (15, [])])])])])])]));;
let drzewo3 = (N (5, []));;		
			
			
			
			
			
			
			
