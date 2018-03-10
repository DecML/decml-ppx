open Decml
open Decml_optimisation
open Decml_linalg

open Csv
open List
open Printf


(* Load data, excluding headers.*)
let raw_data = List.tl (Csv.load "examples/multivariate_regression/bh.csv");;
(* Process input as floats. Organise in (x, y) pairs.*)
let data =  List.map (fun datapoint -> 
	let rev_datapoint = List.rev datapoint in
	let x = List.map float_of_string (List.tl rev_datapoint) in
	(* Pad dataset with 1's for value corresponding to bias term. *)
	let x = [1.0] @ x in
	let y = float_of_string (List.hd rev_datapoint) in
	(x, y)) raw_data
;;

let dot = [%lift dot_product_vectors] in
let (f, p) = [%model let w = [%pm (14, 1)] in
					fun x ->  dot w x] in
(* Perform gradient descent. *)
let learning_rate = 0.000003 in
let epochs = 5000 in
let updated_params = gradient_descent f p mse_loss_function data learning_rate epochs ~suppress_print:false in
(* Can now use optimised version of model to obtain output. *)
f updated_params

