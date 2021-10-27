%left BINOP 
%nonassoc UNOP 
%right LANGLEBRACKET RANGLEBRACKET
%start<Ast.program> entry

%{

open Ast
open Core.AstUtils
open Core.Label
open Core

let make_contract x binders cfs = 
    let union_opt opt1 opt2 = 
        match opt1, opt2 with
        | None, None -> None
        | ((Some _) as opt), None | None, ((Some _) as opt) -> opt 
        | _ -> opt2 (* the last one win *)
    in
    List.fold_left 
        (fun c cf -> {c with 
            ensures= union_opt c.ensures cf.ensures;
            invariant= union_opt c.invariant cf.invariant;
            returns= union_opt c.returns cf.returns;
        })
        {method_name=x; pre_binders=binders; ensures=None; invariant=None; returns=None} 
        cfs

%}

%%

(* -------------------------------------------------------------------------- *)

(* A toplevel phrase is just a term. *)

entry:
  t = list(any_toplevel_term) EOF
    { t }

(* -------------------------------------------------------------------------- *)

(* The syntax of terms is stratified as follows:
    TODO
   atomic_term             -- unambiguously delimited terms
   application_term        -- n-ary applications of atomic terms
   operation_term          -- built using an operation over term(s)
   any_term                -- everything

   A [match/with/end] construct is terminated with an [end] keyword, as in Coq,
   so it is an atomic term. *)
