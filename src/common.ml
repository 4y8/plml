let rec remove_assoc_all x = function
    [] -> []
  | (a, _) as p :: tl -> if a = x
                         then remove_assoc_all x tl
                         else p :: remove_assoc_all x tl

let (@:) l l' =
  l @ (List.filter (fun x -> not (List.mem x l)) l')

let index e l =
  let rec aux n e = function
      [] -> -1
    | hd :: _ when hd = e -> n
    | _ :: tl -> aux (n + 1) e tl
  in aux 0 e l
