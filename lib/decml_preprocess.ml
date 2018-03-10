open List
open Array

let standardize_data data =
    let len = List.length (fst (List.nth data 0)) in

    let mins = Array.make len infinity in
    let maxs = Array.make len neg_infinity in

    let compute_ranges datapoint =
        let update_for_datapoint i value =
            let _ = if (value < Array.get mins i) then Array.set mins i value else () 
            in
            if (value > Array.get maxs i) then Array.set maxs i value else ()
        in
        List.iteri update_for_datapoint (fst datapoint)
    in
    let _ = List.iter compute_ranges data in
    

    let standardize_entry i value = 
        let min = Array.get mins i in
        let max = Array.get maxs i in
        if max = min then (max /. min) else
        (value -. min) /. (max -. min)
    in
    let standardize_row row = 
        List.mapi standardize_entry row
    in
    List.map (fun (x, y) -> ((standardize_row x), y)) data