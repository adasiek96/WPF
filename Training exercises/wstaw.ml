type expr =
  | NWD of expr * expr
  | NWW of expr * expr
  | Number of int
;;
exception Mam
;;
let wstaw (ex, n) =
  let rec nwd a b =
    if a = b then a else
      if a > b then nwd (a - b) b
      else nwd a (b - a)
  in
    let nww a b =
      a * b / nwd a b
    and wynik = ref 0
    in
      let rec f t k =
        match t with
          | NWD (t1, t2) -> nwd (f t1 k) (f t2 k)
          | NWW (t1, t2) -> nww (f t1 k) (f t2 k)
          | Number 0 -> k
          | Number x -> x
      in
        begin try
          for i = 1 to n do
            if f ex i = n then
              begin
                wynik := i;
                raise Mam
              end
          done
          with Mam -> ()
        end;
        !wynik
;;
let t = NWW (NWW (Number 2, Number 0), NWD (Number 0, Number 49));;
let n = 42;;
assert (wstaw (t, n) = 21);;
