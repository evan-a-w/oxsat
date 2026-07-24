open! Core
open! Import

module Axiom = struct
  type t =
    { guard : Formula.any
    ; bound : Tvar.t list
    ; triggers : Formula.any list list
    ; body : Formula.any
    }
  [@@deriving sexp_of]
end

let rec query_of_term ~(bound : Tvar.t list) (term : Formula.any)
  : Formula_egraph.Pattern.Query.t
  =
  match term with
  | Var v when List.mem bound v ~equal:Tvar.equal ->
    Formula_egraph.Pattern.Query.Var (Tvar.to_string v)
  | _ ->
    Formula_egraph.Pattern.Query.App
      (Formula.op term, List.map (Formula.args term) ~f:(query_of_term ~bound))
;;

let substitution_of_match
  (egraph_uf : Formula_egraph_uf.t)
  ~(bound : Tvar.t list)
  (m : Formula_egraph.Pattern.Match.t)
  : Formula.any Tvar.Map.t option
  =
  List.fold_until
    bound
    ~init:[]
    ~f:(fun acc v ->
      match Formula_egraph.Pattern.Subst.find m.subst (Tvar.to_string v) with
      | None -> Stop None
      | Some id ->
        (match Formula_egraph_uf.term_of_id egraph_uf id with
         | None -> Stop None
         | Some term -> Continue ((v, term) :: acc)))
    ~finish:(fun acc -> Some (Tvar.Map.of_alist_exn acc))
;;
