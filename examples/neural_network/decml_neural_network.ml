open Decml
open Decml_optimisation
open Decml_linalg
open Decml_preprocess

open Csv
open List
open Printf


(* Load data, excluding headers.*)
let raw_data = List.tl (Csv.load "examples/neural_network/breastcancerdata.csv");;
(* Process input as floats. Organise in (x, y) pairs.*)
(* TODO: Process properly and do feature scaling... *)
let data = 
	let processed_data =  List.map (fun datapoint -> 
	(* Drop participant id *)
	let datapoint = List.tl datapoint in
	let y = let c = List.hd datapoint in (if String.equal c "M" then 1.0 else 0.0) in
	let x = List.map float_of_string (List.tl datapoint) in
	(* Pad dataset with 1's for value corresponding to bias term. *)
	let x = [1.0] @ x in
	(x, y)) raw_data in
	standardize_data processed_data
;;

let dot, add, sigmoid, softmax = [%lift dot_product_vector_matrix], [%lift add_vectors], [%lift sigmoid], [%lift softmax] in
(* 31 input layers, 10 hidden layers, 2 possible classes. *)
let input_size = 31 in
let hidden_size = 30 in
let output_size = 2 in

let w_1 = [%pm (31, 30)] in
let w_2 = [%pm (30, 2)] in

let (f, p) = [%model fun x -> 
							let z_1 = dot x w_1 in
							let a1 = sigmoid z_1 in
							let z2 = dot a1 w_2 in
							softmax z2]
							in
(* Perform gradient descent. *)
let learning_rate = 0.0001 in
let epochs = 5000 in
let updated_params = gradient_descent f p cross_entropy_loss_function data learning_rate epochs ~suppress_print:false in
(* Can now use optimised version of model to obtain output. *)
f updated_params

