let read_int _ = Scanf.scanf " %d" (fun x -> x)

module IntOrder = struct type t = int let compare = Pervasives.compare end
module IntMap = Map.Make(IntOrder)

let get map key = try IntMap.find key map with Not_found -> 0
let update map key = IntMap.add key ((get map key) + 1) map

let empty_map = IntMap.empty
let array_to_map arr = Array.fold_right (fun x map -> update map x) arr empty_map

let () = 
    let n = read_int () in
    let a = Array.init n read_int in
    let m = read_int () in
    let b = Array.init m read_int in
    let map_a = array_to_map a in
    let map_b = array_to_map b in
    IntMap.iter (fun key _ ->
        if ((get map_a key) < (get map_b key)) then Printf.printf "%d " key) map_b  
