open Decml_optimisation
open Decml
open Csv
open List
open Printf

(* Load data, excluding headers.*)
let raw_data = List.tl (Csv.load "examples/confidence_interval/cats_data.csv");;
(* Process input as floats. Organise in (x, y) pairs.*)
let data =  List.map (fun datapoint -> ((float_of_string (List.nth datapoint 0)), (float_of_string (List.nth datapoint 1)))) raw_data

let square x = x *. x

let is_outside_boundary bound_1 bound_2 datapoint =
	let x, y = datapoint in
	let y_1 = bound_1 x in
	let y_2 = bound_2 x in

	(y_1 < y && y_2 < y) ||
	(y_1 > y && y_2 > y)


let measure_line_distance func data = 
	let datapoint = fst (List.nth data 0) in

	let model_output = func datapoint in
	let first_boundary_y = fst model_output in
	let second_boundary_y = snd model_output in

	sqrt (abs_float ((square first_boundary_y) -. (square second_boundary_y)))


(* Loss function of CI model, as quantified on data.*)
let ci_loss_function func data  =
	let bound_1 = fun x -> (fst (func x)) in
	let bound_2 = fun x -> (snd (func x)) in

	let points_outside_boundary = List.filter (is_outside_boundary bound_1 bound_2) data in

	let mse_1 = mse_loss_function bound_1 points_outside_boundary in
	let mse_2 = mse_loss_function bound_2 points_outside_boundary in

	let avg_mse = (mse_1 +. mse_2) /. 2.0 in
	let distance = measure_line_distance func data in
	avg_mse +. (square distance);;


(* Lift operators to be used in model body. *)
let ( * ), (+), a, m_1, m_2 = [%lift ( *. )], [%lift (+.)], [%pc 1.0], [%pc 1.0], [%pc 0.0] in
(* Define confidence interval model, as per DecML syntax. *)
let (f, p) = [%model fun x -> (a * x + m_1, a * x + m_2)] in
(* Perform gradient descent. *)
let learning_rate = 0.001 in
let epochs = 5000 in
let updated_params = gradient_descent f p ci_loss_function data learning_rate epochs in
(* Can now use optimised version of model to obtain output. *)
f updated_params

