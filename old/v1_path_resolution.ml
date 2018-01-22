(* simple prototype of a path resolution algorithm *)


(* paths ------------------------------------------------------------ *)

type string_component = string (* with no slash FIXME enforced? *)


type component = {
  name: string_component;
  followed_by_slash: bool
}
(* a string_component, possibly followed by a slash *)

type comp_ops = {
  get_comp: string -> component * string
}



(* filesystem ------------------------------------------------------- *)

type file_id

type dir_id

type resolve_result = 
  | File of file_id | Dir of dir_id | Sym of string | Missing 


type fs_ops = {
  root: dir_id;
  resolve_name: dir_id -> string_component -> resolve_result
}


(* path resolution --------------------------------------------------- *)

type path = Abs of string | Rel of string 
(* absolute or relative; string "//abc" is Abs"/abc"; string can be
   empty *)


type state = {
  cwd: dir_id;
  paths: path list;  
  (* never contains an Abs "" or Rel ""? Abs "" is ok for a symlink to
     root; Rel "" is OK to indicate we have reached the end *)
}

let is_finished s = 
  s.paths = []  (* nothing left to resolve *)
  || List.for_all (fun x -> x = Abs "") s.paths
(* NOTE during path res, we may get a file, not followed by a slash,
   but where there are some remaining Abs "" in paths *)


(* if not finished, then there is an Abs or Rel entry at head of
   paths; Abs get converted into Rels *)
let get_next ~comp_ops ~fs_ops s = 
  assert (not (is_finished s));
  match s.paths with
  | [] -> failwith "impossible"
  | hd::rest -> (
      match hd with
      | Abs s' -> 
        (* NOTE we do not update the cwd at this point *)
        (`Root, {s with paths=(Rel s')::rest })
      | Rel s' -> 
        (* s' may be empty *)
        match s' with 
        | "" -> 
          (* no need to change dir in this case *)
          (`Noop, {s with paths=rest})
        | _ -> 
          let (c,s'') = comp_ops.get_comp s' in
          let s = {s with paths=(Rel s'')::rest } in
          fs_ops.resolve_name s.cwd c.name |> fun rr ->
          (`Resolve_result(rr,c.followed_by_slash), s))

let step ~comp_ops ~fs_ops s =
  s |> get_next ~comp_ops ~fs_ops |> function
  | `Root,s -> Ok {s with cwd=fs_ops.root}
  | `Noop,s -> Ok s
  | `Resolve_result(rr,followed_by_slash),s ->
    match followed_by_slash,rr with 
    | true, File _ -> 
      (* NOTE missing/ is ok if the target is a missing directory, say? *)
      Error (`File_followed_by_slash)
    | true, Sym _ ->
      (* if name refers to a symlink, but we have a trailing slash (eg
         a/b/c.txt where b is a symlink), then we need to somehow
         incorporate this information; and it may even matter whether
         this trailing slash is considered as part of the symlink
         resolution, or part of the remaining resolution after the
         symlink has been resolved; for the moment we assume that any
         resolution with further path components corresponds to a
         slash? *)

;;

(* this makes it perhaps even more appropriate to use a sequence of
   names and slashes, or at least, ability to get a name, and if not
   empty, a slash and maybe a next name etc *)


(* operations might be: push rel_string; push abs_string; pop (name or slash or root)

better: push root; push string; pop (name or slash or root)

   the observation is simply that root has to originate from the original
   string, or as the initial slash in a symlink

*)




Slash arises when we resolve 
   we need to record explicitly that there is a slash FIXME but we don't
   want this to be identified somehow as root; *)
