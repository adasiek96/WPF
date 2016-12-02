(* PRZELEWANKA *)
(* Autor: Adam Sobecki; Recenzent: Michał Pawłowski *)


(* FUNKCJE *)

(* [Brak_rozwiazania] - wyjątek podnoszony, gdy jesteśmy w stanie
   stwierdzić, że rozwiązanie nie istnieje *)
exception Brak_rozwiazania

(* [nwd a b] - funkcja zwracająca wartość NWD liczb [a] i [b] *)
let rec nwd a b =
  if a = 0 then b
  else nwd (b mod a) a

(* [moze_istnieje_rozw tab tabx taby] - funkcja sprawdzająca, czy warunki
   konieczne do istnienia rozwiązania zostały spełnione, tj.:
   1) przynajmniej jeden kubek musi być pusty bądź pełny pod koniec
      wykonywania wszystkich operacji przelewania
   2) NWD ([x_1], [x_2], ... , [x_n]) dzieli wszystkie z [y_i] *)
let moze_istnieje_rozw tab tabx taby =
  try let b = ref false in
    Array.iter (fun (x, y) ->
      if (x, y) != (0, 0) then
        if (x = y || y = 0) then b := true) tab;
    if !b = false then raise Brak_rozwiazania else
      begin
        let m = Array.fold_left nwd 0 tabx in
          Array.iter (fun y ->
            if (y mod m) != 0 then raise Brak_rozwiazania) taby;
          true;
      end
  with Brak_rozwiazania -> false

(* [bfs (tab, wyn) tabx taby hash q n] - funkcja sprawdza, czy znaleźliśmy
   rozwiązanie; jeśli nie to dodaje wszystkie wychodzące konfiguracje
   [(tab, wyn)] do kolejki [q], a także tablicę bieżącej konfiguracji [tab]
   do tablicy hashującej [hash]; jeśli dana konfiguracja już się
   powtórzyła, nie jest rozpatrywana po raz kolejny; [wyn] spamiętuje
   liczbę wykonanych operacji na kubkach; ogółem istnieją trzy
   możliwości, po wykonaniu których zapisujemy nasze konfiguracje
   wychodzące:
   1) wylanie całej wody z wybranej szklanki do zlewu
   2) nalanie do wybranej szklanki do pełna wody z kranu
   3) przelanie wody z jednej szklanki do drugiej *)
let bfs (tab, wyn) tabx taby hash q n =
  if Hashtbl.mem hash tab then -1 else
    if tab = taby then wyn else
      begin
        Hashtbl.add hash tab true;
        (* przypadek 1 - wylanie całej wody *)
        for i = 0 to n - 1 do
          if tab.(i) != 0 then begin
            let n_tab = Array.copy tab in
              n_tab.(i) <- 0;
              Queue.push (n_tab, wyn + 1) q
          end
        done;
        (* przypadek 2 - nalanie wody do pełna *)
        for i = 0 to n - 1 do
          if tab.(i) != tabx.(i) then begin
            let n_tab = Array.copy tab in
              n_tab.(i) <- tabx.(i);
              Queue.push (n_tab, wyn + 1) q
          end
        done;
        (* przypadek 3 - przelanie ze szklanki i do szklanki j *)
        for i = 0 to n - 1 do
          if tabx.(i) != 0 then
            for j = 0 to n - 1 do
              if i != j && tab.(j) != tabx.(j) then begin
                let n_tab = Array.copy tab in
                  if n_tab.(i) + n_tab.(j) <= tabx.(j) then begin
                    n_tab.(j) <- n_tab.(i) + n_tab.(j);
                    n_tab.(i) <- 0
                  end
                  else begin
                    n_tab.(i) <- n_tab.(i) - (tabx.(j) - n_tab.(j));
                    n_tab.(j) <- tabx.(j)
                  end;
                Queue.push (n_tab, wyn + 1) q;
              end
            done
        done;
        -1;
      end

(* [przelewanka tab] - funkcja, która mając daną tablicę par liczb
   [|(x1, y1); (x2, y2); ...; (xn, yn)|] wyznacza minimalną liczbę
   czynności potrzebnych do uzyskania sytacji takiej, że w każdej szklance
   znajduje się określona ilość wody, odpowiednio y1, y2, ..., yn;
   jeżeli jej uzyskanie nie jest możliwe, to poprawnym wynikiem jest -1 *)
let przelewanka tab =
  let n = Array.length tab
  and q = Queue.create ()
  in
    let tabx = Array.init n (fun y -> fst tab.(y))
    and taby = Array.init n (fun x -> snd tab.(x))
    and tab_pocz = Array.make n 0
    and hash = Hashtbl.create 42
    in
      Queue.push (tab_pocz, 0) q;
      let wynik = ref (-1) in
        if tabx = tab_pocz then 0 else
          if tabx = taby
          then
            Array.fold_left
              (fun a x -> if x = 0 then a else a + 1) 0 tabx
          else
            if moze_istnieje_rozw tab tabx taby = false then -1
            else begin
              while not (Queue.is_empty q) && !wynik <= 0 do
                wynik := bfs (Queue.pop q) tabx taby hash q n;
              done;
              !wynik;
            end
;;

(* TESTY )

assert (przelewanka [| (10,2); (1,1) |] = 5);;
assert (przelewanka [| (0,0); (2,2); (2,2); (2,2); (0,0); (0,0); (1,0);
  (0,0); (1,0) |] = (3));;
assert (przelewanka [| (1,1); (2,1); (3,0); (4,2) |] = (3));;
assert (przelewanka [| (0,0); (2,2); (1,0); (1,1); (1,0); (2,2); (1,0);
  (0,0); (0,0) |] = (3));;
assert (przelewanka [| (11,11); (11,1) |] = (-1));;
assert (przelewanka [| (1,1); (0,0); (2,2); (0,0); (2,0); (0,0); (0,0);
  (1,0); (2,0); (1,0) |] = (2));;
assert (przelewanka [| (5,2); (0,0); (0,0); (2,0); (3,2) |] = (4));;
assert (przelewanka [| (1,1); (0,0); (4,4); (4,0); (4,4) |] = (3));;
assert (przelewanka [| (9,9); (13,12) |] = (10));;
assert (przelewanka [| (2,2); (1,0); (2,2); (0,0); (1,0); (0,0); (1,1);
  (1,0); (0,0) |] = (3));;
assert (przelewanka [| (5,2); (3,1); (0,0); (4,1); (0,0); (1,0) |] = (5));;
assert (przelewanka [| (310,76); (139,91) |] = (-1));;
assert (przelewanka [| (48,9); (12,0); (1,1); (65,64) |] = (10));;
assert (przelewanka [| (7,5); (3,3); (9,4); (10,4); (6,3); (5,3) |] =
  (8));;
assert (przelewanka [| (100000,50000); (1,1) |] = (100000));;
assert (przelewanka [| (0,0); (0,0); (0,0); (300000,151515);
  (1,0); (0,0) |] = (296971));;
assert (przelewanka [| (11,2); (11,10); (4,0); (10,8); (21,16) |] = (12));;
assert (przelewanka [| (50,1); (7,3); (78,64) |] = (-1));;
assert (przelewanka [| (85,23); (524,210) |] = (-1));;
assert (przelewanka [| (557,349); (73,49) |] = (-1));;
assert (przelewanka [| (62,3); (38,7) |] = (-1));;
assert (przelewanka [| (15,15); (6,3); (42,32); (33,20) |] = (-1));;
assert (przelewanka [| (39,12); (35,34); (21,7); (2,1) |] = (-1));;
assert (przelewanka [| (1,0); (2,1); (2,1); (0,0); (2,0); (0,0); (0,0);
  (0,0); (1,1); (0,0); (1,0) |] = (4));;
assert (przelewanka [| (2,0); (2,2); (2,1); (6,6); (0,0) |] = (-1));;
assert (przelewanka [| (2,0); (1,1); (1,1); (1,1); (0,0); (1,0); (3,2);
  (0,0) |] = (4));;
assert (przelewanka [| (1,1); (2,2); (4,1); (0,0); (1,0); (2,1) |] = (5));;
assert (przelewanka [| (1,0); (3,1); (2,2); (1,1); (1,0); (1,0) |] = (3));;
assert (przelewanka [| (20,7); (12,11) |] = (-1));;
assert (przelewanka [| (0,0); (21,21) |] = (1));;
assert (przelewanka [| (13,8); (11,11) |] = (14));;
assert (przelewanka [| (1,1); (3,2); (6,5) |] = (5));;
assert (przelewanka [| (4,4); (7,6); (2,2) |] = (6));;
assert (przelewanka [| (3,2); (3,3); (1,1); (2,0) |] = (3));;
assert (przelewanka [| (0,0); (2,0); (0,0); (2,0); (3,2); (2,1); (1,0) |] =
  (3));;
( *)
