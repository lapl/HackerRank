let read_int () = Scanf.scanf " %d" (fun x -> x)
let read_string () = Scanf.scanf " %s" (fun x -> x)

let () =
    let cases = read_int () in
    for i = 1 to cases do
        let str = ref (read_string ()) in
        let n = String.length !str in
        for j = 1 to n do
            str := (String.sub !str 1 (n-1)) ^ (Char.escaped !str.[0]);
            Printf.printf "%s " !str;
        done;
        Printf.printf "\n";
    done
