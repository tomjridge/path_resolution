(** Main types; probably don't open *)

(* path components -------------------------------------------------- *)


(* path_component is a bit lengthy, so call it comp_ *)
type comp_ = string (* with no slash FIXME enforced? *)


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


(* FIXME maybe rename this to dir_lookup or similar *)
(** Operations we require from the underlying filesystem; essentially
   we need to be able to look up a component in a directory *)
type ('file_id,'dir_id,'t) fs_ops = {
  root: 'dir_id;
  resolve_comp: 'dir_id -> comp_ -> (('file_id,'dir_id) resolved_comp,'t) m
}

(** The result of path resolution (simplified to reduce complexity) *)
module Simplified_result = struct

  (** A "successful" resolution gives one of the following *)
  type ('file_id,'dir_id) simplified_result' = 
      File of 'file_id | Dir of 'dir_id | Sym of string | Missing

  (** We also need additional information about the resolved result,
     such as the containing directory, the last path component, and
     whether there was a trailing slash on the path *)
  type ('file_id,'dir_id) simplified_result = { 
    parent_id: 'dir_id; 
    comp: comp_; 
    result: ('file_id,'dir_id) simplified_result'; 
    trailing_slash:bool 
  }
end
include Simplified_result


(* NOTE make this polyvar so we don't have to open a module *)
(** During path resolution, we need control over whether the last
   component should be followed if it is a symlink *)
type follow_last_symlink = [ `Always | `If_trailing_slash | `Never ]


(** During path resolution we maintain a state:
- cwd, the current directory
- is_absolute, whether we are working with an absolute path
- path, the path itself, as a string (but without a leading slash,
  which is dealt with by is_absolute)

*)
type 'dir_id state = {
  cwd: 'dir_id;
  is_absolute: bool;
  path: string;  
}


type ('file_id,'dir_id) resolved_err = [ 
  (* FIXME clarify further? *)
  | `File_followed_by_slash_etc of 'dir_id state * comp_ * 'file_id 
                                    
  | `Missing_slash_etc of comp_ * 'dir_id * string ]

type ('file_id,'dir_id) resolved_path_or_err = 
  (('file_id,'dir_id) Simplified_result.simplified_result,
   ('file_id,'dir_id) resolved_err) 
    result

