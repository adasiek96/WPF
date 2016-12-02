exception Stop
;;
let trojkat tab =
  let m = Array.length tab
  and n = Array.length tab.(0) in
  
  let gora = ref 0
  and dol = ref (m-1)
  and lewo = ref (n-1)
  and prawo = ref 0
  and licznik = ref 0 in
  
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        if tab.(i).(j) = true then
          begin
            gora := max !gora i;
            dol := min !dol i;
            lewo := min !lewo j;
            prawo := max !prawo j;
          end
      done
    done;
    
    if tab.(!gora).(!lewo) = true
    then licznik := !licznik + 1;
    if tab.(!gora).(!prawo) = true
    then licznik := !licznik + 1;
    if tab.(!dol).(!lewo) = true
    then licznik := !licznik + 1;
    if tab.(!dol).(!prawo) = true
    then licznik := !licznik + 1;
    
    if !licznik = 0 || !licznik = 4
    then false
    else true
;;
let tab1 =
  [|
    [|false; false; false|];
    [|false; true; false|];
    [|true; true; true|];
    [|false; true; false|]
  |]
;;
let tab2 =
  [|
    [|false; false; false|];
    [|false; true; false|];
    [|false; true; true|];
    [|false; true; false|]
  |]
;;
let tab3 =
  [|
    [|false; false; false|];
    [|false; true; true|];
    [|false; true; true|];
    [|false; false; false|]
  |]
;;
let tab4 =
  [|
    [|false; false; false|];
    [|false; true; false|];
    [|true; true; false|];
    [|false; false; false|]
  |]
;;
assert (trojkat tab4 = true);;
assert (trojkat tab3 = false);;
assert (trojkat tab2 = true);;
assert (trojkat tab1 = false);;
