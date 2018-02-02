type comp_ = string

type ('file_id,'dir_id) resolve_result = 
  | File of 'file_id | Dir of 'dir_id | Sym of string | Missing 

type ('file_id,'dir_id) fs_ops = {
  root: 'dir_id;
  resolve_comp: 'dir_id -> comp_ -> ('file_id,'dir_id) resolve_result  
}

type 'dir_id state = {
  cwd: 'dir_id;
  is_absolute: bool;
  path: string;  
}

type ('file_id,'dir_id) simplified_result' = File of 'file_id | Dir of 'dir_id | Sym of string | Missing

type ('file_id,'dir_id) simplified_result = 
    { parent_id: 'dir_id; comp: comp_; result: ('file_id,'dir_id) simplified_result'; trailing_slash:bool }

val resolve: 
  fs_ops:('file_id,'dir_id) fs_ops ->
  follow_last_symlink:bool ->
  cwd:'dir_id ->
  string ->
  (('file_id,'dir_id) simplified_result,
   [> `File_followed_by_slash_etc of 'dir_id state * comp_ * 'file_id  (* FIXME clarify further? *)
   | `Missing_slash of comp_ * 'dir_id * string ]) result

