(* filesystem ------------------------------------------------------- *)

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

type ('file_id,'dir_id) resolve_result = 
  | File of 'file_id | Dir of 'dir_id | Sym of string | Missing 


type ('file_id,'dir_id) fs_ops = {
  root: 'dir_id;
  resolve_comp: 'dir_id -> comp_ -> ('file_id,'dir_id) resolve_result  
}
