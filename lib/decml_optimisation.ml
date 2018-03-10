open Decml
open Map
open List
open Printf

exception ItemNotFoundException of string

let find_max v = match v with
        | [] -> raise (ItemNotFoundException "Not Found")
        | x::xs -> List.fold_left  (fun a b -> if a > b then a else b) x xs

let find_min v = match v with
        | [] -> raise (ItemNotFoundException "Not Found")
        | x::xs -> List.fold_left  (fun a b -> if a < b then a else b) x xs

let square x = x *. x


let mse_datapoint_loss func datapoint = 
    let x = fst datapoint in
    let y = snd datapoint in
    let error = y -. ((func) x) in
    square error

(* Loss function (MSE) of linear model, as quantified on data.
    Assumes data is structured in an (x, y) tuple, where:
        x - predictor(s), regardless of type
        y - response variable, a float 
*)
let mse_loss_function func data  =
    let data_length = List.length data in
        if data_length == 0 then 0.0
        else
    let sum_terms = List.map (mse_datapoint_loss func) data in
    let sum = List.fold_left (+.) 0.0 sum_terms in
    let n = float (List.length data) in
    (1.0 /. (2.0 *. n)) *. sum;;


(* Defined as H(y, y^) = (y * log y^) + ((1 - y) * log(1-y^)) *)
let cross_entropy_loss_function func data  =
    let compute_datapoint_loss datapoint = 
        let x = fst datapoint in
        let y = snd datapoint in
        let probabilities = func x in
        let y_hat = find_max probabilities in
        (y *. log y_hat) +. ((1.0 -. y) *. log (1.0 -. y_hat))
    in
    let sum_terms = List.map compute_datapoint_loss data in
    let sum = List.fold_left (+.) 0.0 sum_terms in
    let n = float_of_int (List.length data) in
    -. (sum /. n);;

(* 
Compute_partial derivative of the cost function wrt. a particular parameter. 
The calculation is done using the two-sided formula:
f′(x) = (f(x+h) − f(x−h)) / 2h *)
let compute_partial_derivative model parameters loss_function k value datapoint =
    let h = 0.00001 in
    let params_plus = Dict.add k (value +. h) parameters in
    let params_minus = Dict.add k (value -. h) parameters in
    let cost_function_value_plus = loss_function (model params_plus) [datapoint] in
    let cost_function_value_minus = loss_function (model params_minus) [datapoint] in
    (cost_function_value_plus -. cost_function_value_minus) /. (2.0 *. h)


(* Update model parameters by estimating loss function gradient with finite difference approximation. *)
let update_parameters model parameters loss_function datapoint  learning_rate  =
    let update_parameter k value =
        value -. (learning_rate *. (compute_partial_derivative model parameters loss_function k value datapoint))
    in
    (* Get new updated parameter values *)
    Dict.mapi update_parameter parameters


let shuffle ls =
    let nd = List.map (fun c -> (Random.bits (), c)) ls in
    let sond = List.sort compare nd in
    List.map snd sond


(* Stochastic gradient descent routine. *)
let gradient_descent ?suppress_print:(s = false) model parameters loss_function data learning_rate epochs =
    let data = shuffle data in
    let len = List.length data in
    let rec gradient_descent_step model parameters loss_function shuffled_data learning_rate curr_epoch epochs =
        (* Compute loss. Given a reasonable learning rate, it should decrease with each iteration *)
        let current_loss = loss_function (model parameters) data in
        let _ = if s then () else 
            let _ = printf "Step %d. Current loss: %.10f.\n" curr_epoch current_loss in
            printf "%!"
        in

        (* Update parameters via gradient descent. *)
        let datapoint = List.hd shuffled_data in
        let updated_parameters = update_parameters model parameters loss_function datapoint learning_rate in
        (* Stop after a particular number of steps.
        An alternative condition would be to stop once change in loss is below some threshold. *)
        if (curr_epoch = epochs) 
        then updated_parameters
        else 
            if (curr_epoch = len) then gradient_descent_step model updated_parameters loss_function (List.tl shuffled_data) learning_rate (curr_epoch + 1) epochs
                                       else gradient_descent_step model updated_parameters loss_function data learning_rate (curr_epoch + 1) epochs
    in
    gradient_descent_step model parameters loss_function data learning_rate 0 epochs;;