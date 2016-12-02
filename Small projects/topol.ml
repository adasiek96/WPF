(* SORTOWANIE TOPOLOGICZNE *)
(* Autor: Adam Sobecki; Recenzent: Gabriela Gierasimiuk *)


(* FUNKCJE *)
open PMap

(* wyjątek podnoszony przez funkcję [topol], gdy zależności są cykliczne *)
exception Cykliczne

(* [nowa_mapa g] funkcja, która dla zadanej listy tworzy mapę taką, że
wartością każdego wierzchołka jest para [(flaga, lista)], gdzie [flaga]
to stan odwiedzenia (0, 1 lub 2), a [lista] to lista sąsiadujących
wierzchołków *)
let nowa_mapa g =
  List.fold_left
    (fun a (v, l) ->
      add v (0, l) a)
    empty g

(* [odwiedzony v g] zwraca 0 w przypadku gdy wierzchołek nie został jeszcze
odwiedzony, 1 - gdy jest w trakcie operacji odwiedzania i 2 - gdy został
już odwiedzony *)
let odwiedzony v g =
  fst (find v g)

(* [topol lista_g] funkcja, która dla zadanej listy [lista_g] =
[(a_1,[a_11;...;a_1k]);...;(a_m,[a_m1;...;a_mk])] zwraca listę, na której
każdy z elementów [a_i] oraz [a_ij] występuje dokładnie raz, i która jest
uporządkowana w taki sposób, że każdy element [a_i] jest przed każdym
z elementów [a_i1],...,[a_il]; można założyć, że elementy [a_1],...,[a_m]
nie powtarzają się, jak również, że każda z list
[a_11;...;a_1n],...,[a_m1;...;a_mk] ma niepowtarzające się elementy *)
let topol lista_g =
  let wynik = ref []
  and g = ref (nowa_mapa lista_g) in
    let rec dfs v =
      if mem v (!g) then
        let lk = snd (find v (!g)) in
          match odwiedzony v (!g) with
            | 2 -> ()
            | 1 -> raise Cykliczne
            | 0 ->
                g := add v (1, lk) (!g);
                List.iter dfs lk;
                wynik := (v :: (!wynik));
                g := add v (2, lk) (!g)
            | _ -> assert false
      else
        begin
          g := add v (2, []) (!g);
          wynik := (v :: (!wynik))
        end
    in
      let rec loop lista_g =
        match lista_g with
          | [] -> ()
          | (v, l) :: t ->
              dfs v;
              loop t
      in
        loop lista_g;
        !wynik
;;

(* TESTY )
let lista1 =
  [(1, [2; 6]); (2, [3; 4]); (3, [5]); (4, [5]); (5, [6]); (8, [7; 3]);
    (7, [6])]

let lista2 =
  [("skarpetki", ["buty"]); ("buty", ["kurtka"]);
    ("majtki", ["spodnie"]); ("spodnie", ["buty"]);
      ("koszulka", ["bluza"; "kurtka"; "szalik"]);
        ("bluza", ["kurtka"; "szalik"]);
          ("kurtka", ["czapka"; "rekawiczki"]);
            ("szalik", ["kurtka"])]

let lista3 = [(1, [2]); (2, [1]); (3, [1])]

let lista4 =
  [(1, [5; 8; 3]); (2, [5; 6]); (3, [5; 2; 8]); (5, [7]);
    (6, [8]); (7, [8])]

let lista5 =
  [(1, [5; 8; 3]); (2, [5; 6]); (3, [5; 2; 8]); (5, [7]);
    (6, [8]); (7, [8]); (9, [8]); (4, [])]

let lista6 =
  [(1, [5; 8; 3]); (2, [5; 6]); (3, [5; 2; 8; 10]); (5, [7]);
    (6, [8]); (7, [8]); (9, [8; 2]); (10, [9])]

let lista7 =
  [(1, [5; 8; 3]); (2, [5; 6]); (3, [5; 2; 8; 10]); (5, [7; 9]);
    (6, [8]); (7, [8]); (9, [8; 2]); (10, [9])]
;;

assert (topol lista1 = [8; 7; 1; 2; 4; 3; 5; 6]);;
assert (topol lista2 = ["koszulka"; "bluza"; "szalik"; "majtki"; "spodnie";
  "skarpetki"; "buty"; "kurtka"; "rekawiczki"; "czapka"]);;
assert ((try (topol lista3) with Cykliczne -> [0]) = [0]);;
assert (topol lista4 = [1; 3; 2; 6; 5; 7; 8]);;
assert (topol lista5 = [4; 9; 1; 3; 2; 6; 5; 7; 8]);;
assert (topol lista6 = [1; 3; 10; 9; 2; 6; 5; 7; 8]);;
assert ((try (topol lista7) with Cykliczne -> [0]) = [0]);;

(...*)
