let lamiglowka lista =
  let n = List.length lista in
  let hash = Hashtbl.create n in
  let rec pom l =
    match l with
      | [] -> Hashtbl.length hash
      | h :: t ->
        if Hashtbl.mem hash h
        then
          begin
            Hashtbl.remove hash h;
            pom ((2 * h) :: t)
          end
        else
          begin
            Hashtbl.add hash h h;
            pom t
          end
  in pom lista
;;
assert (lamiglowka [4; 3; 4; 5; 12; 2; 3; 1; 6; 1] = 4);;