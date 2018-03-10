open Ast_helper
open Asttypes
open Parsetree
open Longident

let keycount = ref 0

let generate_key = fun () ->
  let value = !keycount in
  let _ = incr keycount in
  string_of_int value

let construct_var_identifier = fun varname -> ({ppat_desc = Ppat_var {txt = varname; loc=(!default_loc)}; ppat_attributes = []; ppat_loc=(!default_loc)})

let fresh_var_index = ref 0
                                      
let gen_var_name = fun () ->
  let value = (!fresh_var_index) in
  let _ = incr fresh_var_index in
  String.concat "_" ["q"; string_of_int value]

let reset_fresh_var_index = fun () ->
  fresh_var_index := 0


let extract_matrix_dim exp =
  let loc = exp.pexp_loc in
  begin match exp.pexp_desc with
    | Pexp_constant (Pconst_integer (int_value, _)) -> int_of_string int_value
    | _ -> raise (Location.Error (Location.error ~loc "Bad provisional matrix dimension. Should be a positive integer."))
  end

let rec reconstruct_list merged_params = match merged_params with
  | [] -> Exp.construct ({txt =  Lident "[]"; loc=(!default_loc)}) None
  | hd :: tl -> Exp.construct ({txt =  Lident "::"; loc=(!default_loc)})
                              (Some (Exp.tuple ~loc:(!default_loc) [hd; reconstruct_list tl]))

and construct_dict_recursively dicts_to_union = 
  match dicts_to_union with
    | [] -> Exp.ident {txt = Ldot (Lident "Dict", "empty"); loc=(!default_loc)}
    | dict_to_union :: [] -> dict_to_union
    | dict_to_union :: rest -> Exp.apply (Exp.ident {txt = Ldot (Lident "Dict", "union"); loc=(!default_loc)}) [(Nolabel, dict_to_union); (Nolabel, construct_dict_recursively rest)]
    | _ -> raise (Location.Error (Location.error ~loc:(!default_loc) "Illegal argument when trying to union parameter dictionaries"))


(* Functions below were used in a previous version of the translation for function application. *)
(* Somewhat difficult to write, therefore I am leaving them here for (possible) future use. *)
(* Dead code - use with care. *)
let rec find_free_variables function_variable_name body = 
  match body with
  | {pexp_loc = loc;
    pexp_desc = exp_desc} ->
      begin match exp_desc with
        | Pexp_ident {txt = Lident variable_name} -> 
            if (variable_name <> function_variable_name && variable_name <> "pc") then [body] else []
        | Pexp_apply (function_to_apply, function_arguments) -> 
          (find_free_variables function_variable_name function_to_apply) @ List.flatten (List.map (fun x -> find_free_variables function_variable_name (snd x)) function_arguments)
        | Pexp_tuple tuple_elements -> List.flatten (List.map (fun x -> find_free_variables function_variable_name x) tuple_elements)
        | _ -> []
      end

let rec find_transformed_free_variable variable_name transformed_free_variables =
  match transformed_free_variables with
    | [] -> None
    | transformed_free_variable :: tail -> 
      begin match transformed_free_variable.pexp_desc with
        | Pexp_tuple tuple_elements -> let free_var_function = List.nth tuple_elements 0 in
            match free_var_function.pexp_desc with
              | Pexp_fun (labels, attrs, pattern, free_var_identifier) -> 
                match free_var_identifier.pexp_desc with
                  | Pexp_ident {txt = Lident transformed_free_variable_name} ->
                    if transformed_free_variable_name == variable_name then Some transformed_free_variable
                    else find_transformed_free_variable variable_name tail
                  | _ -> None
              | _ -> None
        | _ -> find_transformed_free_variable variable_name tail
      end

let rec find_provisional_constants body =
  match body with
  | {pexp_loc = loc;
    pexp_desc = exp_desc} ->
      begin match exp_desc with
        | Pexp_extension ({txt = "pc"; loc }, pstr) ->
          begin match pstr with
            | PStr [{ pstr_desc =
                      Pstr_eval (exp, _)}] -> [body]
          end
        | Pexp_apply (function_to_apply, function_arguments) -> 
                begin match function_to_apply.pexp_desc with
                  | _ -> List.flatten (List.map (fun x -> find_provisional_constants (snd x)) function_arguments)
                end
        | Pexp_tuple tuple_elements -> List.flatten (List.map (fun x -> find_provisional_constants x) tuple_elements)
        | _ -> []
      end

let get_dict_from_tuple exp = match exp.pexp_desc with
 | Pexp_tuple [func; dict] -> dict
 | _ -> raise (Location.Error (Location.error ~loc:(!default_loc) "Bad transformed node. Expected function and dict"))

let rec merge_dictionaries transformed_free_variables transformed_provisional_constants =
  let p_s = List.map get_dict_from_tuple transformed_free_variables in
  let q_s = List.map get_dict_from_tuple transformed_provisional_constants in
  let merged_params = p_s @ q_s in
  let merged_params_exp = reconstruct_list merged_params in
  Exp.apply (Exp.ident {txt = Ldot (Lident "List", "fold_left"); loc=(!default_loc)})
            [(Nolabel, Exp.ident {txt = Ldot (Lident "Dict", "union"); loc=(!default_loc)});
             (Nolabel, Exp.ident {txt = Ldot (Lident "Dict", "empty"); loc=(!default_loc)});
             (Nolabel, merged_params_exp)]

let print_extracted_provisional_constants provisional_constants = 
  let print_pc_application x = 
    match x.pexp_desc with
      Pexp_extension ({txt = "pc"; loc }, pstr) ->
          match pstr with
            | PStr [{ pstr_desc =
                      Pstr_eval (exp, _)}] -> match exp.pexp_desc with Pexp_constant Pconst_float (value, _) -> value in
          
  let provisional_constant_values = List.map print_pc_application provisional_constants in
  let () = Printf.printf("Provisional constants extracted as: \n") in 
  let () = List.iter (Printf.printf "[pc %s];")  provisional_constant_values in
  let () = Printf.printf("\n") in
  ()

let print_extracted_free_variables free_vars =
  let extracted_var_names = List.map (fun x -> match x.pexp_desc with Pexp_ident {txt = Lident varname} -> varname) free_vars in
  let () = Printf.printf("Free variable names extracted as: \n") in 
  let () = List.iter (Printf.printf "%s;") extracted_var_names in
  let () = Printf.printf("\n") in
  ()
