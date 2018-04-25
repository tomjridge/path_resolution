(* filesystem ------------------------------------------------------- *)

open Tjr_step_monad

open Path_component

(* 

NOTE the following two types are really private - they can be ints
or whathaveyou; for the purposes of easy testing, they are
strings 

To avoid functorization, we use type vars

*)
(*
type file_id = Private_file_id of string
type dir_id = Private_dir_id of string
*)

type ('file_id,'dir_id) resolved_comp = 
  | RC_file of 'file_id | RC_dir of 'dir_id | RC_sym of string | RC_missing 


type ('file_id,'dir_id,'t) fs_ops = {
  root: 'dir_id;
  resolve_comp: 'dir_id -> comp_ -> (('file_id,'dir_id) resolved_comp,'t) m
}
