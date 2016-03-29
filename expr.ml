(** Type des expressions. *)
type t =
  | Var of string
  | Int of int
  | String of string
  | Let of string * t * t
  | App of string * t list

(** Utilitaire pour afficher des listes. *)
let pp_list pp_item pp_sep chan l =
  let rec aux = function
    | [] -> ()
    | [e] -> pp_item chan e
    | hd::tl -> pp_item chan hd ; pp_sep chan ; aux tl
  in aux l

(** Afficheur d'expressions. *)
let rec pp_expr chan = function
  | Var v -> Format.fprintf chan "%s" v
  | Int i -> Format.fprintf chan "%d" i
  | String s -> Format.fprintf chan "%S" s
  | Let (x,v,b) ->
      Format.fprintf chan
        "@[<2>let %s = @,%a@, in @,%a@]" (* improve TODO *)
        x
        pp_expr v
        pp_expr b
  | App (f,args) ->
      Format.fprintf chan
        "%s(%a)"
        f
        (pp_list pp_expr (fun chan -> Format.fprintf chan ",@,")) args
