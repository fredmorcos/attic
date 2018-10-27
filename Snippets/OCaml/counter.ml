open Core.Std

(* type t = (string * int) list *)

(* let empty = [] *)

(* let to_list x = x *)

(* let touch t s = *)
(*   let count = *)
(*     match List.Assoc.find t s with *)
(*     | None -> 0 *)
(*     | Some x -> x *)
(*   in List.Assoc.add t s (count + 1) *)

type t = int String.Map.t

let empty = String.Map.empty

let to_list t = Map.to_alist t

let touch t s =
  let count =
    match Map.find t s with
    | None -> 0
    | Some x -> x
  in Map.add t ~key:s ~data:(count + 1)
