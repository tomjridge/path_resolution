
open Tjr_fs_shared.Monad

(** Path components ie strings without slash *)
type comp_ = string


(** Result of resolving a single path component *)
type ('file_id,'dir_id) resolve_result = 
  | File of 'file_id | Dir of 'dir_id | Sym of string | Missing 


(** What we need from the filesystem *)
type ('file_id,'dir_id,'t) fs_ops = {
  root: 'dir_id;
  resolve_comp: 'dir_id -> comp_ -> (('file_id,'dir_id) resolve_result,'t) m
}


(** The state maintained during path resolution; any returned state
    should have is_absolute=false, so this is only useful for cwd and
    path *)
type 'dir_id state = {
  cwd: 'dir_id;
  is_absolute: bool;
  path: string;  
}


(** The result of path resolution... *)
type ('file_id,'dir_id) simplified_result' = File of 'file_id | Dir of 'dir_id | Sym of string | Missing

(** ... *)
type ('file_id,'dir_id) simplified_result = 
    { parent_id: 'dir_id; comp: comp_; result: ('file_id,'dir_id) simplified_result'; trailing_slash:bool }


(** The resolve function itself. 

Errors:
- File_followed_by_slash_etc: f.txt/some/more/stuff or f.txt// (not f.txt/)
- Missing_slash_etc: /a/b/missing/some/more/stuff

*)
val resolve: 
  fs_ops:('file_id,'dir_id,'t) fs_ops ->
  follow_last_symlink:bool ->
  cwd:'dir_id ->
  string ->
  ((('file_id,'dir_id) simplified_result,
   [> `File_followed_by_slash_etc of 'dir_id state * comp_ * 'file_id 
   | `Missing_slash_etc of comp_ * 'dir_id * string ]) result,
   't) m

