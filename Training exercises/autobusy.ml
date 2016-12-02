exception Stop
;;
let sufit x y =
  if y = 0 then 0 else
    if x mod y = 0 then x / y
    else x / y + 1
;;
let autobusy tab1 tab2 =
  let licz1 = ref 0
  and licz2 = ref 0
  and wyn = ref 0
  and n = Array.length tab1
  and m = Array.length tab2 in
    let k = ref (m - 1) in
      (try
        for i = n - 1 downto 0 do
          if !licz1 > 0 && !licz2 = 0 then begin
            wyn := -1; raise Stop end;
          if tab2.(!k) = i then begin
            licz2 := !licz2 + 1;
            wyn := max !wyn (sufit !licz1 (!licz2 - 1));
            if !k - 1 >= 0 then k := !k - 1 end;
          licz1 := !licz1 + tab1.(i);
        done;
        wyn := max !wyn (sufit !licz1 !licz2);
      with Stop -> ());
      !wyn
;;
let tab1 = [|1; 0; 2; 1; 2; 0; 1; 2; 6; 0; 6; 0|];;
let tab2 = [|2; 6; 8; 11|];;
assert (autobusy tab1 tab2 = 7);;
