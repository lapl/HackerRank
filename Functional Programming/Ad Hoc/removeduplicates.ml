let read_line () = Scanf.scanf " %s" (fun x -> x)

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

let () = 
    let str = read_line () in
    let s = explode str in
    let answer = List.fold_left (fun y x -> if (List.mem x y) then y else (x :: y)) [] s in
    Printf.printf "%s" (implode (List.rev answer)) 
