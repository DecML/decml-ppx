open Decml
open Decml_optimisation
open Csv
open List
open Printf

(* Load data, excluding headers.*)
let raw_data = List.tl (Csv.load "examples/linear_regression/cats_data.csv");;
(* Process input as floats. Organise in (x, y) pairs.*)
let data =  List.map (fun datapoint -> ((float_of_string (List.nth datapoint 0)), (float_of_string (List.nth datapoint 1)))) raw_data;;


(* Lift operators to be used in model body. *)
let ( * ), (+), a, b = [%lift ( *. )], [%lift (+.)], [%pc 0.0], [%pc 0.0] in

(* Define linear regression model, as per DecML syntax. *)
let (f, p) = [%model fun x -> a * x + b] in
(* Perform gradient descent. *)
let learning_rate = 0.0001 in
let epochs = 5000 in
let updated_params = gradient_descent f p mse_loss_function data learning_rate epochs ~suppress_print:false in
(* Can now use optimised version of model to obtain output. *)
f updated_params

