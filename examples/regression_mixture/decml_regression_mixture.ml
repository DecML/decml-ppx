open Decml
open Decml_optimisation

open Csv
open List
open Printf

(* Load data, excluding headers.*)
let raw_data = List.tl (Csv.load "examples/regression_mixture/regression_mixture.csv");;
(* Process input as floats. Organise in (x, y) pairs.*)
let data =  List.map (fun datapoint -> ((float_of_string (List.nth datapoint 0)), (float_of_string (List.nth datapoint 1)))) raw_data


let rec sublist b e l = 
  match l with
  |  [] -> failwith "index error"
  | h :: t -> 
     let tail = if e=0 then [] else sublist (b-1) (e-1) t in
     if b>0 then tail else h :: tail


let pick_slices first_model_losses second_model_losses data =
	let rec pick_slices' first_model_losses second_model_losses data first_slice second_slice = 
		match first_model_losses, second_model_losses, data with
			| [], [], []-> (first_slice, second_slice)
			| l1::tl1, l2::tl2, d::tld -> 
				if (l1 -. l2) < 0.0 then pick_slices' tl1 tl2 tld (first_slice @ [d]) second_slice
				else pick_slices' tl1 tl2 tld first_slice (second_slice @ [d])
			| _ -> failwith "illegal arguments"
	in
	pick_slices' first_model_losses second_model_losses data [] []


let report_total_loss first_model_losses second_model_losses step_number =
	let rec compute_total_loss first_model_losses second_model_losses sum =
		match first_model_losses, second_model_losses with
			| [], [] -> sum
			| l1::tl1, l2::tl2 -> 
				if (l1 -. l2) < 0.0 then compute_total_loss tl1 tl2 (sum +. l1)
				else compute_total_loss tl1 tl2 (sum +. l2)
			| _ -> failwith "illegal arguments"
	in
	let total_loss = compute_total_loss first_model_losses second_model_losses 0.0 in
	let _ = printf "Total loss at meta step %d: %.5f\n" step_number total_loss in
	(* Flush output *)
	printf "%!"


let rec fit_lines l1_model l1_params l2_model l2_params l1_slice l2_slice step_number meta_steps =
	(* Do gradient descent twice: once on first slice of data, second time on second slice. *)
	let learning_rate = 0.00001 in
	let epochs = 1000 in
	let l1_updated_params = gradient_descent l1_model l1_params mse_loss_function l1_slice learning_rate epochs ~suppress_print:true in
	let l2_updated_params = gradient_descent l2_model l2_params mse_loss_function l2_slice learning_rate epochs ~suppress_print:true in

	let first_model_losses = List.map (mse_datapoint_loss (l1_model l1_updated_params)) data in
	let second_model_losses = List.map (mse_datapoint_loss (l2_model l2_updated_params)) data in

	let updated_first_slice, updated_second_slice = pick_slices first_model_losses second_model_losses data in
	let () = report_total_loss first_model_losses second_model_losses step_number in

	if step_number = meta_steps then ((l1_model, l1_updated_params), (l2_model, l2_updated_params))
	else fit_lines l1_model l1_updated_params l2_model l2_updated_params updated_first_slice updated_second_slice (step_number + 1) meta_steps


let optimise_params l1 l2 = 
	let (l1_model, l1_params) = l1 in
	let (l2_model, l2_params) = l2 in

	(* Split in two slices *)
	let n = List.length data in
	let first_data_slice = sublist 0 (n/2) data in
	let second_data_slice = sublist (n/2 + 1) (n-1) data in
	
	let meta_steps = 500 in
	fit_lines l1_model l1_params l2_model l2_params first_data_slice second_data_slice 0 meta_steps


;;

(* Lift operators to be used in model body. *)
let ( * ), (+), a_1, a_2, b_1, b_2 = [%lift ( *. )], [%lift (+.)], [%pc 0.0], [%pc 0.0], [%pc 0.0], [%pc 0.0] in

(* Define the two weighted regression models, as per DecML syntax. *)
let l1 = [%model fun x -> (a_1 * x + b_1)] in
let l2 = [%model fun x -> (a_2 * x + b_2)] in

(* Optimise params *)
let opt_l1, opt_l2 = optimise_params l1 l2 in
(* Construct weighted regression model *)
let (f, p) = [%model fun x -> (opt_l1 x, opt_l2 x)] in
(* Can now use optimised version of model to obtain output. *)
f p

