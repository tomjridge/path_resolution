(** Main types; don't open - comp_ is aliased to string *)


(** During path resolution we maintain a state:
- cwd, the current directory
- is_absolute, whether we are working with an absolute path
- path, the path itself, as a string (but without a leading slash,
  which is dealt with by is_absolute)

*)
type 'did state = {
  cwd         : 'did;
  is_absolute : bool;
  path        : string;  
}


module Public = struct

  (** Path components *)
  type comp_ = string (* with no slash FIXME enforced? *)

  (** fid - file id; did - dir id; sid - symlink id *)
  type ('fid,'did,'sid) resolved_comp = 
    | RC_file of 'fid | RC_dir of 'did | RC_sym of 'sid*string | RC_missing 


  (** Operations we require from the underlying filesystem; essentially
      we need to be able to look up a component in a directory *)
  type ('fid,'did,'sid,'t) fs_ops = {
    root: 'did;
    resolve_comp: 'did -> comp_ -> (('fid,'did,'sid) resolved_comp,'t) m
  }


  (* NOTE make this polyvar so we don't have to open a module *)
  (** During path resolution, we need control over whether the last
      component should be followed if it is a symlink *)
  type follow_last_symlink = [ `Always | `If_trailing_slash | `Never ]


  (** The result of path resolution (simplified to reduce complexity). A
      "successful" resolution gives one of the following *)
  type ('fid,'did,'sid) simplified_result' = 
      File of 'fid | Dir of 'did | Sym of 'sid*string | Missing

  (** We also need additional information about the resolved result,
      such as the containing directory, the last path component, and
      whether there was a trailing slash on the path *)
  type ('fid,'did,'sid) simplified_result = { 
    parent_id      : 'did; 
    comp           : comp_; 
    result         : ('fid,'did,'sid) simplified_result'; 
    trailing_slash : bool 
  }

  type ('fid,'did) resolved_err = [ 
    (* FIXME clarify further? *)
    | `File_followed_by_slash_etc of 'did state * comp_ * 'fid 

    | `Missing_slash_etc of comp_ * 'did * string ]

  type ('fid,'did,'sid) resolved_path_or_err = 
    (('fid,'did,'sid) simplified_result,
     ('fid,'did) resolved_err) 
      result

end
include Public
