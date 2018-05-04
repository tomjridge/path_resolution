(** Interface to a specification/implementation of path resolution. *)


(* FIXME we may want to expose some of the functions in String_util *)

open Tjr_monad.Monad

(** Path components ie strings without slash. *)
type comp_ = string


(** Result of resolving a single path component. *)
(* FIXME rename resolved_comp; also avoid clash with later type? or at least split into a separate module *)
type ('file_id,'dir_id) resolved_comp = 
  | RC_file of 'file_id | RC_dir of 'dir_id | RC_sym of string | RC_missing 



(** What we need from the filesystem. *)
type ('file_id,'dir_id,'t) fs_ops = {
  root: 'dir_id;
  resolve_comp: 'dir_id -> comp_ -> (('file_id,'dir_id) resolved_comp,'t) m
}



(** The state maintained during path resolution; any user-visible (ie
    returned to user from this library) state should have
    is_absolute=false, so the only user-visible components that are
    useful are cwd and path *)
(* FIXME could make abstract, with projection functions *)
type 'dir_id state = {
  cwd: 'dir_id;
  is_absolute: bool;
  path: string;  
}


(** The result of path resolution... *)
type ('file_id,'dir_id) simplified_result' = 
    File of 'file_id | Dir of 'dir_id | Sym of string | Missing



(** 

We have the following information about a resolved path:

- [parent_id] is the directory which contains the result
- [comp] the name of the result within the parent
- [result] the simplified result (file,dir,sym or missing)
- [trailing_slash] whether there was a **single** trailing slash left to process

*)
type ('file_id,'dir_id) simplified_result = { 
  parent_id: 'dir_id; 
  comp: comp_; 
  result: ('file_id,'dir_id) simplified_result'; 
  trailing_slash:bool 
}


(** Different commands resolve symlinks in different ways. This allows
    control over that behaviour, also taking into account whether there is
    a trailing slash or not. *)
type follow_last_symlink = [ `Always | `If_trailing_slash | `Never ]

(** The resolve function itself. 

Errors:
- File_followed_by_slash_etc: f.txt/some/more/stuff or f.txt// (not f.txt/, which resolves to a file with [trailing_slash] true); NOTE that f.txt// is sometimes considered acceptable eg on Mac OS X I believe FIXME check
- Missing_slash_etc: /a/b/missing/some/more/stuff

*)
val resolve: 
  monad_ops: 't monad_ops ->
  fs_ops:('file_id,'dir_id,'t) fs_ops ->
  follow_last_symlink:follow_last_symlink ->
  cwd:'dir_id ->
  string ->
  ((('file_id,'dir_id) simplified_result,
   [> `File_followed_by_slash_etc of 'dir_id state * comp_ * 'file_id 
   | `Missing_slash_etc of comp_ * 'dir_id * string ]) result,
   't) m

