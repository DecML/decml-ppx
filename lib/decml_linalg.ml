open Decml

open List
open Printf

exception ListLengthDifferenceException of string
exception ItemNotFoundException of string

let find_max v = match v with
        | [] -> raise (ItemNotFoundException "Not Found")
        | x::xs -> List.fold_left  (fun a b -> if a > b then a else b) x xs

let find_min v = match v with
        | [] -> raise (ItemNotFoundException "Not Found")
        | x::xs -> List.fold_left  (fun a b -> if a < b then a else b) x xs
        
let dot_product_vectors v1 v2 =
    let rec dot_product' v1 v2 acc = 
        match v1, v2 with
            | [], [] -> acc
            | v1h::v1tl, v2h::v2tl -> let newacc = (v1h *. v2h) +. acc in
                                        dot_product' v1tl v2tl newacc
            | _, _ -> raise (ListLengthDifferenceException "Lists differ in length")
    in dot_product' v1 v2 0.0;;

let dot_product_vector_matrix v m =
    let result = List.map (fun x -> dot_product_vectors v x) m in
    (* let _ = Printf.printf "DOT PRODUCT RESULT: [" in
    let _ = List.iter (Printf.printf "%5.3f;") result in
    let _ = Printf.printf "]\n" in *)
    result

let add_vectors v1 v2 = 
    List.map2 (+.) v1 v2

let sigmoid v = 
    let sigmoid' x = 
        if x > 500.0 then 1.0 
                    else (exp x) /. (exp x +. 1.0) in
    List.map sigmoid' v

let softmax v =
    let exponents = List.map exp v in
    let sum = List.fold_left (+.) 0.0 exponents in
    let softmaxes = List.map (fun x -> x /. sum) exponents in
(*  let _ = List.iter (Printf.printf "SOFTMAX: %5.3f\n") softmaxes in
 *) softmaxes

let argmax v = 
    let rec find x lst = match lst with
        | [] -> raise (ItemNotFoundException "Not Found")
        | hd::tl -> if x = hd then 0 else 1 + find x tl
    in
    let max_val = find_max v in
    find max_val v;;
