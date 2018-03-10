open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Map
open Set
open List

open Decml_ppx_utils

module VariableSet = Set.Make(String)

let rec root_translater exp =
  let _ = reset_fresh_var_index () in
  match exp with 
  | {pexp_loc  = loc;
    pexp_desc = exp_desc} ->
      begin match exp_desc with
        | Pexp_extension ({txt = "lift"; loc }, pstr) -> lift_translater exp (VariableSet.empty)
        | Pexp_extension ({txt = "pc"; loc }, pstr) -> pc_translater exp (VariableSet.empty)
        | Pexp_extension ({txt = "pm"; loc }, pstr) -> pm_translater exp (VariableSet.empty)
        | Pexp_extension ({txt = "model"; loc }, pstr) -> 
          begin match pstr with
            | PStr [{ pstr_desc =
                      Pstr_eval (expression, _)}] -> model_translater expression (VariableSet.empty)
            | _ -> raise (Location.Error (Location.error ~loc "Syntax error in translater"))
          end 
        | _ -> raise (Location.Error (Location.error ~loc "Syntax error in root translater"))
      end

and lift_translater exp env =
  match exp with 
  | {pexp_loc  = loc;
    pexp_desc = exp_desc} ->
      begin match exp_desc with
        | Pexp_extension ({txt = "lift"; loc }, pstr) ->
          begin match pstr with
            | PStr [{ pstr_desc =
                      Pstr_eval (exp, _)}] -> translate_lift_expr exp env
            | _ -> raise (Location.Error (Location.error ~loc "Syntax error when translating lift expression"))                       
          end          
        | _ -> raise (Location.Error (Location.error ~loc "Syntax error in lift translater"))
      end

and pc_translater exp env =
  match exp with 
  | {pexp_loc  = loc;
    pexp_desc = exp_desc} ->
      begin match exp_desc with
        | Pexp_extension ({txt = "pc"; loc }, pstr) ->
          begin match pstr with
            | PStr [{ pstr_desc =
                      Pstr_eval (exp, _)}] -> translate_provisional_constant_expr exp env
            | _ -> raise (Location.Error (Location.error ~loc "Syntax error when translating provisional constant"))                       
          end
        | _ -> raise (Location.Error (Location.error ~loc "Syntax error in pc translater"))
      end

and pm_translater exp env = 
  match exp with
  | {pexp_loc  = loc;
    pexp_desc = exp_desc} ->
      begin match exp_desc with
        | Pexp_extension ({txt = "pm"; loc }, pstr) ->
          begin match pstr with 
            |  PStr [{pstr_desc =
                 Pstr_eval (exp, _)}] -> translate_provisional_matrix_expr exp env
            | _ -> raise (Location.Error (Location.error ~loc "Syntax error when translating provisional matrix"))                       
          end
        | _ -> raise (Location.Error (Location.error ~loc "Syntax error in pm translater"))
      end     

and model_translater exp env = 
  match exp with 
  | {pexp_loc  = loc;
    pexp_desc = exp_desc} ->
      begin match exp_desc with
        | Pexp_ident identifier_expr -> translate_variable_expression exp env
        | Pexp_extension ({txt = "lift"; loc }, pstr) -> lift_translater exp env      
        | Pexp_extension ({txt = "pc"; loc }, pstr) -> pc_translater exp env
        | Pexp_extension ({txt = "pm"; loc }, pstr) -> pm_translater exp env
        | Pexp_tuple exp -> translate_tuple_expr exp env
        | Pexp_apply (function_to_apply, function_arguments) -> 
                begin match function_to_apply.pexp_desc with
                  | Pexp_ident {txt = Lident "dec"} -> raise (Location.Error (Location.error ~loc "in-model decoupling not yet implemented"))
                  | _ -> translate_function_application_expr function_to_apply function_arguments env
                end
        | Pexp_fun (_, _, variable_pattern, body) -> translate_function_definition_expr variable_pattern body env
        | Pexp_let (rec_flag, pattern, body) -> translate_let_expr rec_flag pattern body env
        | Pexp_ifthenelse (if_exp, then_exp, else_exp) -> translate_ifthenelse_expr if_exp then_exp else_exp env
        | Pexp_construct (constructor_node, arguments) -> translate_constructor_expr constructor_node arguments env
        | _ -> raise (Location.Error (Location.error ~loc "Syntax error in model translator"))
      end


and translate_lift_expr exp env = 
  let f = Exp.fun_ Nolabel None (Pat.any ()) exp in
  let dict = Exp.ident {txt = Ldot (Lident "Dict", "empty"); loc=(!default_loc)} in 
  Exp.tuple [f; dict]

and translate_provisional_constant_expr provisional_constant_expr env =
    let loc = provisional_constant_expr.pexp_loc in
    begin match provisional_constant_expr.pexp_desc with
      | Pexp_constant Pconst_float (value, _) -> 
                             let var_id = (gen_var_name ()) in
                             let l = Exp.constant (Pconst_integer (generate_key (), None)) in
                             let p = Exp.apply (Exp.ident {txt = Ldot (Lident "Dict", "add"); loc=(!default_loc)}) 
                                     [(Nolabel, l);
                                      (Nolabel, Exp.constant  (Pconst_float (value, None)));
                                      (Nolabel, Exp.ident {txt = Ldot (Lident "Dict", "empty"); loc=(!default_loc)})] in
                             let lookup = Exp.apply (Exp.ident {txt = Ldot (Lident "Dict", "find"); loc=(!default_loc)}) 
                                     [(Nolabel, l);
                                      (Nolabel, Exp.ident {txt = Lident var_id; loc=(!default_loc)});] in 
                             let f = Exp.fun_ Nolabel None (construct_var_identifier var_id) lookup in
                             Exp.tuple ~loc [f; p]
      | _ -> raise (Location.Error (Location.error ~loc "Provisional constant must be a float value"))
    end
 
and translate_provisional_matrix_expr provisional_matrix_expr env = 
  let loc = provisional_matrix_expr.pexp_loc in
  begin match provisional_matrix_expr.pexp_desc with
    | Pexp_tuple tuple_elements -> 
        let len = List.length tuple_elements in
        match len with
          | 2 ->  let num_columns = extract_matrix_dim (List.nth tuple_elements 0) in
                  let num_rows = extract_matrix_dim (List.nth tuple_elements 1) in
                  model_translater (construct_provisional_matrix num_columns num_rows) env
          | _ -> raise (Location.Error (Location.error ~loc "Bad tuple size for initializing provisional matrix"))
    | _ -> raise (Location.Error (Location.error ~loc "Syntax error in provisional matrix translation"))
  end

and construct_provisional_matrix num_columns num_rows = 
  let rec construct_row_recursively columns_left = 
    if columns_left == 0 then Exp.construct {txt = Lident "[]"; loc=(!default_loc)} None
    else
      let curr_pc = Exp.constant (Pconst_float (string_of_float (Random.float 1.0), None)) in
      let pc_ext = Exp.extension (({txt = "pc"; loc=(!default_loc)}), PStr [{pstr_loc = (!default_loc); pstr_desc = Pstr_eval (curr_pc, [])}]) in
      let rest = construct_row_recursively (columns_left-1) in
      let tup = Some (Exp.tuple [pc_ext; rest]) in
      Exp.construct {txt = Lident "::"; loc=(!default_loc)} tup
  in
  let rec construct_matrix_recursively num_columns rows_left =
    if rows_left == 0 then Exp.construct {txt = Lident "[]"; loc=(!default_loc)} None
    else
      let curr_row = construct_row_recursively num_columns in
      let rest = construct_matrix_recursively num_columns (rows_left-1) in
      let tup = Some (Exp.tuple [curr_row; rest]) in
      Exp.construct {txt = Lident "::"; loc=(!default_loc)} tup
  in
  if num_rows == 1 then
    construct_row_recursively num_columns
  else  
    construct_matrix_recursively num_columns num_rows

and translate_variable_expression exp env =
  match exp.pexp_desc with 
  | Pexp_ident {txt = Lident var_name} -> 
    if not (VariableSet.mem var_name env) then
      let firstapp = Exp.apply (Exp.ident {txt = Lident "fst"; loc=(!default_loc)}) [(Nolabel, Exp.ident {txt = Lident var_name; loc=(!default_loc)})] in
      let secondapp = Exp.apply (Exp.ident {txt = Lident "snd"; loc=(!default_loc)}) [(Nolabel, Exp.ident {txt = Lident var_name; loc=(!default_loc)})] in
      Exp.tuple [firstapp; secondapp]
    else
      let f = Exp.fun_ Nolabel None (Pat.any ()) exp in
      let dict = Exp.ident {txt = Ldot (Lident "Dict", "empty"); loc=(!default_loc)} in 
      Exp.tuple [f; dict]
  | _ -> raise (Location.Error (Location.error ~loc:exp.pexp_loc "Malformed variable expression"))
    
and translate_tuple_expr tuple_elements env =
  let transformed_tuple_elements = List.map (fun x -> model_translater x env) tuple_elements in
  let f_p_s = List.map (fun x -> extract_transformed_tuple x) transformed_tuple_elements in
  
  let var_id = gen_var_name () in
  let var_ident = Exp.ident {txt = Lident var_id; loc=(!default_loc)} in

  let applications = List.map (fun (f, p) -> Exp.apply f [(Nolabel, var_ident)]) f_p_s in

  let funbody = Exp.tuple applications in
  let f = Exp.fun_ Nolabel None (construct_var_identifier var_id) funbody in

  let dicts_to_union = List.map (fun (f, p) -> p) f_p_s in
  let dict = construct_dict_recursively dicts_to_union in
  Exp.tuple [f; dict]
 
and translate_function_application_expr function_to_apply function_arguments env =
  let transformed_function_to_apply = model_translater function_to_apply env in

  let function_arguments_no_label = List.map (fun x -> snd x) function_arguments in
  let transformed_arguments = List.map (fun x -> model_translater x env) function_arguments_no_label in

  let f_p_s = List.map (fun x -> extract_transformed_tuple x) ([transformed_function_to_apply] @ transformed_arguments) in
  
  let var_id = gen_var_name () in
  let var_ident = Exp.ident {txt = Lident var_id; loc=(!default_loc)} in

  let applications = List.map (fun (f, p) -> Exp.apply f [(Nolabel, var_ident)]) (List.tl f_p_s) in
  let applications_with_label = List.map (fun x -> (Nolabel, x)) applications in

  let firstapp = Exp.apply (fst (List.hd f_p_s)) [(Nolabel, var_ident)] in
  let app = Exp.apply firstapp applications_with_label in
  
  let dicts_to_union = List.map (fun (f, p) -> p) f_p_s in
  let dict = construct_dict_recursively dicts_to_union in
  
  let f = Exp.fun_ Nolabel None (construct_var_identifier var_id) app in
  Exp.tuple [f; dict]
 

and extract_transformed_tuple transformed_tuple_ast_node = 
  begin match transformed_tuple_ast_node.pexp_desc with
    | Pexp_tuple generated_tuple -> (nth generated_tuple 0, nth generated_tuple 1)
    | _ -> (Exp.unreachable (), Exp.ident {txt = Ldot (Lident "Dict", "empty"); loc=(!default_loc)})
  end 

and translate_function_definition_expr variable_pattern body env =
  let variable_name = match variable_pattern.ppat_desc with
                        | Ppat_var {txt = var} -> var
                        (* TODO: Currently, only model definitions in a single, non-pattern matched variable are possible. *)
                        (* e.g., fun (x, y) -> x + y causes below error to be raised. *)
                        | _ -> raise (Location.Error (Location.error ~loc:variable_pattern.ppat_loc "Syntax error in variable pattern"))
  in
  let updated_env = VariableSet.add variable_name env in
  let (f, p) = let tuple_expression = model_translater body updated_env in 
                          begin match tuple_expression.pexp_desc with
                            | Pexp_tuple generated_tuple -> (nth generated_tuple 0, nth generated_tuple 1)
                            | _ -> raise (Location.Error (Location.error ~loc:tuple_expression.pexp_loc "Bad tuple element when processing function arguments"))
                          end
  in
  let var_id = (gen_var_name ()) in
  let app = Exp.apply f [(Nolabel, (Exp.ident {txt = Lident var_id; loc=(!default_loc)}) )] in
  let lambda_x = Exp.fun_ Nolabel None (construct_var_identifier variable_name) app in
  let lambda_q = Exp.fun_ Nolabel None (construct_var_identifier var_id) lambda_x in
  Exp.tuple [lambda_q; p]

and translate_let_expr rec_flag pattern body env =

  let singleton_item = (List.nth pattern 0) in
  match singleton_item with
    | {pvb_pat = variable;
       pvb_expr = value} -> let lambda_fun = (Exp.fun_ Nolabel None variable body) in
                            let desugared = Exp.apply lambda_fun [(Nolabel, value)] in
                            model_translater desugared env
      (* TODO: Currently, only let bindings in a single, non-pattern matched variable are possible. *)
      (* e.g., let (x, y) = foo causes below error to be raised. *)
    | _ -> raise (Location.Error (Location.error ~loc:(!default_loc) "Malformed let expression - only single variables are supported in bindings."))

and translate_ifthenelse_expr if_exp then_exp else_exp_opt env = 
  let f_0, p_0 = extract_transformed_tuple (model_translater if_exp env) in
  let f_1, p_1 = extract_transformed_tuple (model_translater then_exp env) in
  
  let else_exp = 
    match else_exp_opt with
      | Some else_exp_val -> else_exp_val
      | None -> raise (Location.Error (Location.error ~loc:(!default_loc) "Else needs to be specified"))
    in
  let f_2, p_2 = extract_transformed_tuple (model_translater else_exp env) in

  let var_id = gen_var_name () in
  let if_app = Exp.apply f_0 [(Nolabel, Exp.ident {txt = Lident var_id; loc=(!default_loc)})] in
  let then_app = Exp.apply f_1 [(Nolabel, Exp.ident {txt = Lident var_id; loc=(!default_loc)})] in
  let else_app = Exp.apply f_2 [(Nolabel, Exp.ident {txt = Lident var_id; loc=(!default_loc)})] in

  let reconstructed_ifthenelse = Exp.ifthenelse if_app then_app (Some else_app) in
  let lambda_q = Exp.fun_ Nolabel None (construct_var_identifier var_id) reconstructed_ifthenelse in
  let dict = construct_dict_recursively [p_0; p_1; p_2] in
  Exp.tuple [lambda_q; dict]

and translate_constructor_expr constructor_node arguments env =
  match arguments with 
    | None -> translate_lift_expr (Exp.construct constructor_node None) env
    | Some args -> match args.pexp_desc with
      | Pexp_tuple tuple_elements -> 
        let transformed_args = List.map (fun x -> model_translater x env) tuple_elements in

        let is_terminal x = (match x.pexp_desc with | Pexp_construct (const, None) -> true | _ -> false) in
        let non_terminal_args = List.filter (fun x -> not (is_terminal x)) transformed_args in
        let terminal_args = List.filter is_terminal transformed_args in
        let f_p_s = List.map (fun x -> extract_transformed_tuple x) non_terminal_args in

        let var_id = gen_var_name () in
        let var_ident = Exp.ident {txt = Lident var_id; loc=(!default_loc)} in

        let applications = List.map (fun (f, p) -> Exp.apply f [(Nolabel, var_ident)]) f_p_s in
        let applications = applications @ terminal_args in

        let funbody = Exp.construct constructor_node (Some (Exp.tuple applications)) in
        let f = Exp.fun_ Nolabel None (construct_var_identifier var_id) funbody in

        let dicts_to_union = List.map (fun (f, p) -> p) f_p_s in
        let dict = construct_dict_recursively dicts_to_union in
        Exp.tuple [f; dict]
      | _ -> raise (Location.Error (Location.error ~loc:(!default_loc) "Malformed constructor expression"))