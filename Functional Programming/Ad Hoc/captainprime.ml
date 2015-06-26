let read_int _ = Scanf.scanf " %d" (fun x -> x)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l;;

let list_of_int x = explode (string_of_int x)
let int_of_list x = int_of_string (implode x)

let () = 
    let is_prime = Array.make 1000001 true in
    is_prime.(0) <- false;
    is_prime.(1) <- false;
    for i = 2 to 1000000 do
        if is_prime.(i) then begin
            let j = ref (i * i) in
            while !j <= 1000000 do
                is_prime.(!j) <- false;
                j := !j + i;
            done;
        end;
    done;
    
    let contains_0 lst = List.mem '0' (list_of_int lst) in
    
    let rec all_prime lst rev = match lst, rev with
        | _ :: [], _ -> true
        | _ :: body, false -> is_prime.(int_of_list body) && all_prime body rev
        | _ :: body, true -> is_prime.(int_of_list (List.rev body)) && all_prime body rev
    in
    
    let all_prime_left lst = all_prime (list_of_int lst) false in
    let all_prime_right lst = all_prime (List.rev (list_of_int lst)) true in
    
    let solve x = match is_prime.(x), contains_0 x, all_prime_left x, all_prime_right x with
        | true,false,true,true  -> Printf.printf "CENTRAL\n"
        | true,false,true,false -> Printf.printf "LEFT\n"
        | true,false,false,true -> Printf.printf "RIGHT\n"
        | _                   -> Printf.printf "DEAD\n"
    in
    
    let n = read_int () in
    for i = 1 to n do
        let x = read_int () in
        solve x
    done
    
