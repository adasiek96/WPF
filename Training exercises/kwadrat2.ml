exception Stop
;;
let kwadrat tab =
  let m = Array.length tab
  and n = Array.length tab.(0) in
  
    let gora = ref (-1)
    and dol = ref (-1)
    and lewo = ref (-1)
    and prawo = ref (-1)
    and licznik = ref 0 in
    
    (* gora *)
    (try for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        if tab.(i).(j) = true
        then begin gora := i; raise Stop end
      done
    done with Stop -> ());
    
    (* dol *)
    (try for i = m - 1 downto 0 do
      for j = n - 1 downto 0 do
        if tab.(i).(j) = true
        then begin dol := i; raise Stop end
      done
    done with Stop -> ());
    
    (* lewo *)
    (try for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        if tab.(j).(i) = true
        then begin lewo := i; raise Stop end
      done
    done with Stop -> ());
    
    (* prawo *)
    (try for i = n - 1 downto 0 do
      for j = m - 1 downto 0 do
        if tab.(j).(i) = true
        then begin prawo := i; raise Stop end
      done
    done with Stop -> ());
    
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
assert (kwadrat tab4 = true);;
assert (kwadrat tab3 = false);;
assert (kwadrat tab2 = true);;
assert (kwadrat tab1 = false);;
