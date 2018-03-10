open Map
open List
open Printf

exception DictionaryException of string
exception ListLengthDifferenceException of string

module Base_Dict = Map.Make(struct type t = int let compare = compare end)

module Dict = struct
	include Base_Dict
	
	type names = int list
	type vec = float list

	let empty = Base_Dict.empty
	let add = Base_Dict.add
	let merge = Base_Dict.merge
	let bindings = Base_Dict.bindings

	let deconstr dict =
		let rec deconstruct' items (keys, values) = match items with
			| [] -> (keys, values)
			| (key, value)::tail -> deconstruct' tail (key::keys, value::values)
		in
		let all_items = bindings dict in
		let keys, values = deconstruct' all_items ([], []) in
		List.rev keys, List.rev values

	let constr (keys, values) =
		let rec constr' (keys, values) dict = match keys, values with
			| [], [] -> dict
			| key::other_keys, value::other_values -> constr' (other_keys, other_values) (add key value dict)
			| _, _ -> raise (DictionaryException "Bad arguments to constr")
		in
		constr' (keys, values) empty

	let union_criterion key firstval secondval = 
		begin match firstval, secondval with
			| Some x1, None -> firstval
			| None, Some x2 -> secondval
			| Some x1, Some x2 -> firstval
			| None, None -> None
		end
	let union first second = merge union_criterion first second
end
	