let read_int _ = Scanf.scanf " %d" (fun x -> x)

let () = 
    let n = read_int () in
    let m = read_int () in
    let a = Array.init n read_int in
    let h = Array.init n read_int in
    let can number =
        let b = Array.mapi (fun i x -> x + (number - 1) * h.(i)) a in
        Array.fast_sort (fun x x' -> x - x') b;
        let sum = Array.fold_left (fun acc x -> acc + x) 0 (Array.sub b 0 number) in
        sum <= m
    in
    let rec bs lo hi =
        if lo = hi - 1 then lo else 
        begin
            let mid = (lo + hi) / 2 in
            if (can mid) then bs mid hi
                         else bs lo mid
        end
    in
    Printf.printf "%d\n" (bs 0 (n+1))
