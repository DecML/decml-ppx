open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Map
open List

open Decml_ppx_translators

let rec expr_mapper mapper expr = 
   begin match expr with
      | { pexp_desc =
          Pexp_extension ({ txt = tag; loc }, pstr)} ->
        if List.mem tag ["model"; "pc"; "pm"; "lift"] then root_translater expr
        else default_mapper.expr mapper expr
      (* Delegate to the default mapper. *)
      | _ -> default_mapper.expr mapper expr;
  end

and pattern_mapper mapper pattern = 
  begin match pattern with
    | x -> default_mapper.pat mapper x
  end

and decml_mapper argv =
  { 
    default_mapper with
    expr = expr_mapper;
    pat = pattern_mapper
  }
 
let () = register "decml" decml_mapper
