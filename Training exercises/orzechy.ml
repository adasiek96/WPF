let orzechy k a =
   let m = Array.length a
   and n = Array.length a.(0) in
    let pref = Array.make_matrix (m + 1) (n + 1) 0
    and wyn = ref 0 in
      let s (a, b) (c, d) =
      pref.(c).(d) - pref.(a).(d) - pref.(c).(b) + pref.(a).(b) in
        for i = 1 to m do
          for j = 1 to n do
            pref.(i).(j) <- pref.(i - 1).(j) + pref.(i).(j - 1)
              - pref.(i - 1).(j - 1) + a.(i - 1).(j - 1)
          done
        done;
        for a = 1 to k do
          if k mod a = 0 then
            let b = k / a in
              for i = 1 to m - a do
                for j = 1 to n - b do
                  wyn := max !wyn (s (i, j) (i + a, j + b))
                done
              done
        done;
        !wyn
;;
let tab =
  [|[|0; 0; 4; 1; 2; 0|]; [|3; 3; 8; 11; 3; 2|]; [|1; 3; 9; 8; 1; 2|];
    [|2; 1; 1; 2; 0; 12|]|];;
let k = 6;;
orzechy k tab;;
