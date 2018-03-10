open OUnit2
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Decml_ppx_extension
open Decml_ppx_translators
open Decml_ppx_utils

open Decml

(* Tests for utils module *)
let utils_suite = "Utils" >:::
[
    "generate_key" >:: ( fun ctx -> 
        assert_equal "0" (Decml_ppx_utils.generate_key ());
    );

    "generate_var_name" >:: ( fun ctx -> 
        assert_equal "q_0" (Decml_ppx_utils.gen_var_name ());
    );

    "generate_var_name_twice" >:: ( fun ctx -> 
        assert_equal "q_0" (Decml_ppx_utils.gen_var_name ());
        assert_equal "q_1" (Decml_ppx_utils.gen_var_name ());
    );

    "test_extract_matrix_dim" >:: ( fun ctx -> 
        let expr = [%expr 10] in
        let extracted_dimension = extract_matrix_dim expr in
        assert_equal 10 extracted_dimension;
    );

    "find_free_variables_empty" >:: (fun ctx ->
        let free_vars_test_expr = [%expr x] in
        let free_vars = (Decml_ppx_utils.find_free_variables "x" free_vars_test_expr) in
        let list_length = List.length(free_vars) in
        assert_equal 0 list_length;
    );

    "find_free_variables_applications" >:: (fun ctx ->
        let free_vars_test_expr = [%expr x +. y +. z] in
        let free_vars = (Decml_ppx_utils.find_free_variables "x" free_vars_test_expr) in
        let list_length = List.length(free_vars) in
        assert_equal 4 list_length;
    );

    "find_free_variables_tuples" >:: (fun ctx ->
        let free_vars_test_expr = [%expr (x +. y, z)] in
        let free_vars = (Decml_ppx_utils.find_free_variables "x" free_vars_test_expr) in
        let list_length = List.length(free_vars) in
        assert_equal 3 list_length;
    );
]

(* Tests for extension module *)
let extension_suite = "Extension" >:::
[
	"test_lift_primitive_constant" >:: ( fun ctx -> 
		let (f, p) = [%lift 1.0] in
		let result = f p in
		assert_equal 1.0 result;
	);

	"test_lift_variable " >:: ( fun ctx -> 
		let a = 1.0 in
        let (f, p) = [%lift a] in
		let result = f p in
		assert_equal 1.0 result;
	);
  
    "test_provisional_constant_separate" >:: ( fun ctx -> 
        let (f, p) = [%pc 1.0] in
        let result = f p in
        assert_equal 1.0 result;
    );

    "test_provisional_constant" >:: ( fun ctx -> 
        let (f, p) = [%model [%pc 1.0]] in
        let result = f p in
        assert_equal 1.0 result;
    );

    "test_provisional_matrix_1d_shape" >:: ( fun ctx -> 
        let (f, p) = [%pm (10, 1)] in
        let vector = f p in
        assert_equal 10 (List.length vector);
    );
    
    "test_provisional_matrix_1d_values" >:: ( fun ctx -> 
        let (f, p) = [%pm (10, 1)] in
        let vector = f p in
        let is_between_zero_and_one x = (x >= 0.0 && x <= 1.0) in
        List.iter (fun item -> assert_equal true (is_between_zero_and_one item)) vector
    );

    "test_provisional_matrix_2d_shape" >:: ( fun ctx -> 
        let (f, p) = [%pm (20, 10)] in
        let matrix  = f p in
        let _ = assert_equal 10 (List.length matrix) in
        List.iter (fun vector -> assert_equal 20 (List.length vector)) matrix;
    );

    "test_tuple_expression"  >:: ( fun ctx -> 
        let (f, p) = [%model ([%pc 1.0], [%pc 2.0])] in
        let (pc1, pc2) = f p in
        let _ = assert_equal 1.0 pc1 in
        assert_equal 2.0 pc2;
    );

    "test_nested_tuple_expression"  >:: ( fun ctx -> 
        let (f, p) = [%model ([%pc 1.0], ([%pc 2.0], [%pc 3.0]))] in
        let (pc1, (pc2, pc3)) = f p in
        let _ = assert_equal 1.0 pc1 in
        let _ = assert_equal 2.0 pc2 in
        assert_equal 3.0 pc3;
    );

    "test_function_application"  >:: ( fun ctx -> 
        let a, b, (+) = [%lift 1.0], [%lift 2.0], [%lift (+.)] in
        let (f, p) = [%model a + b] in
        let result = f p in
        assert_equal 3.0 result;
    );

    "test_function_application_multiple"  >:: ( fun ctx -> 
        let a, b, c, (+) = [%lift 1.0], [%lift 2.0], [%lift 3.0], [%lift (+.)] in
        let (f, p) = [%model a + b + c] in
        let result = f p in
        assert_equal 6.0 result;
    );

    "test_function_definition"  >:: ( fun ctx -> 
        let a, b, (+), ( * ) = [%lift 1.0], [%lift 2.0], [%lift (+.)], [%lift ( *. )] in
        let (f, p) = [%model fun x -> a * x + b] in
        let result = f p 1.0 in
        assert_equal 3.0 result;
    );

    "test_function_definition_multiple_variables"  >:: ( fun ctx -> 
        let a, (+), ( * ) = [%lift 1.0], [%lift (+.)], [%lift ( *. )] in
        let (f, p) = [%model fun x -> fun y -> a * x + y] in
        let result = f p 1.0 1.0 in
        assert_equal 2.0 result;
    );

    "test_function_definition_multiple_variables_single_fun"  >:: ( fun ctx -> 
        let a, (+), ( * ) = [%lift 1.0], [%lift (+.)], [%lift ( *. )] in
        let (f, p) = [%model fun x y -> a * x + y] in
        let result = f p 1.0 1.0 in
        assert_equal 2.0 result;
    );

    "test_let_expression"  >:: ( fun ctx -> 
        let i, (+) = [%lift 1.0], [%lift (+.)] in
        let (f, p) = [%model let x = i in x + i] in
        let result = f p in
        assert_equal 2.0 result;
    );

    "test_let_expression_multiple"  >:: ( fun ctx -> 
        let i, j, (+) = [%lift 1.0], [%lift 2.0], [%lift (+.)] in
        let (f, p) = [%model let x = i in let y = j in x + i + j] in
        let result = f p in
        assert_equal 4.0 result;
    );

    "test_let_expression_function_definition"  >:: ( fun ctx -> 
        let i, j, (+) = [%lift 1.0], [%lift 2.0], [%lift (+.)] in
        let (f, p) = [%model let f x y = x + y + i + j in f i j] in
        let result = f p in
        assert_equal 6.0 result;
    );

     "test_function_definition_nested_let"  >:: ( fun ctx -> 
        let bar x = x +. 1.0 in
        let foo x = x +. 2.0 in
        let lbar = [%lift bar] in
        let lfoo = [%lift foo] in
        let (+) = [%lift (+.)] in
        let (f, p) = [%model fun x -> let a = lfoo x in let b = lbar x in a + b] in
        let result = f p 3.0 in
        assert_equal 9.0 result;
    );

    "test_if_then_else_step_function"  >:: ( fun ctx -> 
        let (<), c, i, j = [%lift (<)], [%lift 0.0], [%lift 0.0], [%lift 1.0] in
        let (f, p) = [%model fun x -> if (x < c) then i else j] in
        let result = f p 0.5 in
        assert_equal 1.0 result;
    );

    "test_list_constructor_translation_1d"  >:: ( fun ctx -> 
        let (f, p) = [%model [[%pc 1.0]; [%pc 2.0]; [%pc 3.0]; [%pc 4.0]]] in
        let result = f p in
        let _ = assert_equal 1.0 (List.nth result 0) in
        let _ = assert_equal 2.0 (List.nth result 1) in
        let _ = assert_equal 3.0 (List.nth result 2) in
        assert_equal 4.0 (List.nth result 3);
    );

    "test_list_constructor_translation_empty"  >:: ( fun ctx -> 
        let (f, p) = [%model [] ] in
        let result = f p in
        assert_equal 0 (List.length result);
    );

    "test_list_constructor_translation_2d"  >:: ( fun ctx -> 
        let (f, p) = [%model [ [[%pc 1.0]; [%pc 2.0]; [%pc 3.0]; [%pc 4.0]]; []] ] in
        let result = f p in
        assert_equal 2 (List.length result);
    );
]

let suite = 
	"DecML" >:::
		[utils_suite; extension_suite]

(* Test Runner; ~verbose:true gives info on succ tests *)
let _ = run_test_tt_main suite