let read_int _ = Scanf.scanf " %d" (fun x -> x)
let read_string _ = Scanf.scanf " %s" (fun x -> x)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let cache = Array.make 301 (-1)

let rec solve n = match n with
    | -1 | 0 -> 0
    | _ -> if cache.(n) <> -1 then cache.(n) else begin
        let mark = Array.make 301 false in
        for j = 0 to n-1 do
            let result1 = (solve j) lxor (solve (n - j - 1)) in
            let result2 = (solve j) lxor (solve (n - j - 2)) in
            mark.(result1) <- true;
            mark.(result2) <- true;
        done;
        let ret = ref 0 in
        while mark.(!ret) = true do
            incr ret;
        done;
        cache.(n) <- !ret;
        !ret
    end

let () = 
    let testcases = read_int () in
    for case = 1 to testcases do
        let _ = read_int () in
        let s = read_string () in
        let lst = explode s in
        let (grundy, size) = List.fold_right (fun ch (grundy, size) ->
            if ch = 'I' then (grundy, size + 1) else (grundy lxor (solve size), 0)) lst (0,0)
        in
        let grundy = grundy lxor (solve size) in
        let ans = match grundy with
            | 0 -> "LOSE"
            | _ -> "WIN"
        in
        Printf.printf "%s\n" ans;
    done
