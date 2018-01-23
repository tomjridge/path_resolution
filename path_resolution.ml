(* Simple prototype of a path resolution algorithm ------------------- *)

(* NOTE root is special! Not to be confused with '/' appearing inside
   a path *)

(* 

The SibylFS spec contains a description of path resolution,
parameterized for all the crazy behavioural differences (especially on
Mac). 

The following is an attempt to impose a comprehensible structure on
the real-world messiness. The algorithm below executes a version of
path resolution which takes account of the weirdnesses in real-world
behaviour. It is structured so that

- it "naturally" follows some notion of path resolution which is
  general enough to account for real-world behaviour
- it makes minimal commitments at each stage
- it tries to preserve complete information (of course, it drops bits
  of the path that have resolved uncontroversially)
- path resolution is broken down into stages
  - getting the "next" path component
  - updating the state and returning a result
  - finally, implementing path resolution recursively
- small steps are taken!
- what is happening is clear at each step
- the code is parameterized over the filesystem implementation
  (fs_ops)


At the moment, we do not take account of MAX_SYMLINK, MAX_PATH
etc. But these are easy to add.

NOTE one of the problems is the overloading of '/' as a component
separator, the root (not so bad) and an indicator that a symlink
should be resolved

*)


(* strings ---------------------------------------------------------- *)

let slash_char = '/'

let split_on_first_slash s =
  let open String in
  match index s slash_char with
  | exception Not_found -> (s,None)
  | i -> (sub s 0 i, Some(sub s (i+1) (length s - (i+1))))

let drop_first_char s = String.sub s 1 (String.length s -1)

let starts_with_slash str = 
  match str <>"" && String.get str 0 = slash_char with
  | true -> Some (drop_first_char str)
  | false -> None


(* paths ------------------------------------------------------------ *)

(* path_component is a bit lengthy, so call it comp_ *)
type comp_ = string (* with no slash FIXME enforced? *)


(* filesystem ------------------------------------------------------- *)

(* NOTE the following two types are really private - they can be ints
   or whathaveyou; for the purposes of easy testing, they are
   strings *)
type file_id = Private_file_id of string
type dir_id = Private_dir_id of string

type resolve_result = 
  | File of file_id | Dir of dir_id | Sym of string | Missing 


type fs_ops = {
  root: dir_id;
  resolve_name: dir_id -> comp_ -> resolve_result
}


(* path resolution -------------------------------------------------- *)


type state = {
  cwd: dir_id;
  is_absolute: bool;
  path: string;  
}


let is_finished s = 
  not (s.is_absolute) && (s.path = "")


(* if name refers to a symlink, but we have a trailing slash (eg
   a/b/c.txt where b is a symlink), then we need to somehow
   incorporate this information; and it may even matter whether
   this trailing slash is considered as part of the symlink
   resolution, or part of the remaining resolution after the
   symlink has been resolved; for the moment we assume that any
   resolution with further path components corresponds to a
   slash? *)
let state_from_symlink ~cwd ~symlink_contents ~path = 
  let str = symlink_contents in
  match starts_with_slash str with
  | Some str' -> 
    { cwd;
      is_absolute=true; 
      path=(str'^"/"^path) }
  | None -> 
    { cwd;
      is_absolute=false;
      path=str^"/"^path }



(* this deals with strings *)
let get_next_comp ~fs_ops s = 
  assert (not (is_finished s));
  match s.is_absolute with
  | true -> `Root, {s with is_absolute=false}
  | false ->
    (* we know that path <> "" *)
    assert(s.path <> "");
    split_on_first_slash s.path |> fun (comp,rest) ->
    match rest with 
    | None -> `Finished_no_slash comp, {s with path=""}
    | Some "" -> `Finished_slash comp, {s with path=""}
    | Some rest -> `Comp_with_slash comp, {s with path=rest}

let _ = get_next_comp

(* this interacts with the fs *)
let step ~fs_ops s =
  s |> get_next_comp ~fs_ops |> function
  | `Root,s -> `Ok {s with cwd=fs_ops.root}
  | `Finished_no_slash c,s -> 
    (* FIXME try and resolve further? No, that is part of the next stage
       of processing, specific to the syscall involved etc *)
    `Finished_no_slash (c,s.cwd)  
  | `Finished_slash c,s -> `Finished_slash (c,s.cwd)
  | `Comp_with_slash c,s -> 
    (* we know there is more to come, but these may just be extra
       trailing slashes and the current entry may be missing, which is
       potentially a valid case: /a/b//// where b is a missing target
       of a dir rename; if this is valid, this is essentially saying
       that multiple trailing slashes are irrelevant and that a
       trailing slash can be used to indicate a target that is
       non-existing and a directory *)
    (* question if "" should be silently ignored, or actually passed to resolve_name *)
    fs_ops.resolve_name s.cwd c |> function
    | File fid -> 
      (* NOTE missing/ is ok if the target is a missing directory *)
      (* NOTE some platforms allow a file to be referred to with a
         trailing slash *)
      `Error (`File_followed_by_slash (s,c,fid))
    | Dir d ->
      `Ok {s with cwd=d}
    | Sym str -> (
        `Ok (state_from_symlink ~cwd:s.cwd ~symlink_contents:str ~path:s.path))
    | Missing -> 
      (* return all that we know; path may be empty, or "//.../" so
         this could be a valid path *)
      `Missing_slash(c,s)

let _ = step

(* resolve all but last component *)
let resolve_butlast ~fs_ops ~cwd = 
  let step = step ~fs_ops in
  let rec f s = 
    assert (not (is_finished s));
    step s |> function
    | `Ok s -> f s
    | `Finished_no_slash x -> `Finished_no_slash x
    | `Finished_slash x -> `Finished_slash x
    | `Error e -> `Error e
    | `Missing_slash x -> `Missing_slash x
  in  
  fun s ->
    match starts_with_slash s with
    | None -> f { cwd; is_absolute=false; path=s }
    | Some path -> f { cwd; is_absolute=true; path }

(* NOTE that the last component is not resolved; follow_last_symlink
   etc can be done as another step *)
let _ : 
  fs_ops:fs_ops -> cwd:dir_id -> string ->
  [> `Error of [> `File_followed_by_slash of state * comp_ * file_id ]
  | `Finished_no_slash of comp_ * dir_id
  | `Finished_slash of comp_ * dir_id
  | `Missing_slash of comp_ * state ]
  = resolve_butlast

;;

(* resolve_butlast is cumbersome to use if you want information about
   the last component, and want to deal with follow_last_symlink; the
   next piece of code returns info on the last component, but NOTE
   there are several ways implementations commonly differ in
   behaviour *)

let resolve ~fs_ops ~follow_last_symlink ~cwd = 
  let step = step ~fs_ops in
  let rec f s = 
    assert (not (is_finished s));
    step s |> function
    | `Ok s -> f s
    | `Finished_no_slash (comp,dir) -> (
      fs_ops.resolve_name dir comp |> function
      | File fid -> `Finished_no_slash_file (dir,comp,fid)
      | Dir d -> `Finished_no_slash_dir (dir,comp,d)
      | Sym str -> (
        match follow_last_symlink with
        | true -> 
          state_from_symlink ~cwd:dir ~symlink_contents:str ~path:"" |> f
        | false -> 
          `Finished_no_slash_symlink (dir,comp,str))
      | Missing -> `Missing_finished_no_slash(dir,comp))
    | `Finished_slash (comp,dir) -> (
        fs_ops.resolve_name dir comp |> function
        | File fid -> 
          (* potentially an error *)
          `Finished_slash_file (dir,comp,fid)
        | Dir d -> `Finished_slash_dir (dir,comp,d)
        | Sym str -> (
            (* FIXME the following line doesn't hold on all platforms *)
            let follow_last_symlink = true in  (* because trailing slash *)
            match follow_last_symlink with
            | true -> 
              state_from_symlink ~cwd:dir ~symlink_contents:str ~path:"" |> f
            | false -> 
              (* NOTE this case only occurs if the follow_last_symlink
                 is false - which may occur on some platforms *)
              `Finished_slash_symlink (dir,comp,str))
        | Missing -> `Missing_finished_slash(dir,comp))
    | `Error e -> `Error e
    | `Missing_slash (comp,s) -> 
      (* NOTE that there may well be further stuff to resolve in
         s.path, likely indicating an error FIXME resolve this? check
         List.for_all trailing slashes? *)
      `Missing_slash (comp,s)
  in  
  fun s ->
    match starts_with_slash s with
    | None -> f { cwd; is_absolute=false; path=s }
    | Some path -> f { cwd; is_absolute=true; path }

let _ : 
fs_ops:fs_ops ->
follow_last_symlink:bool ->
cwd:dir_id ->
string ->
[> `Error of [> `File_followed_by_slash of state * comp_ * file_id ]
 | `Finished_no_slash_dir of dir_id * comp_ * dir_id
 | `Finished_no_slash_file of dir_id * comp_ * file_id
 | `Finished_no_slash_symlink of dir_id * comp_ * string
 | `Finished_slash_dir of dir_id * comp_ * dir_id
 | `Finished_slash_file of dir_id * comp_ * file_id
 | `Finished_slash_symlink of dir_id * comp_ * string
 | `Missing_slash of comp_ * state
 | `Missing_finished_no_slash of dir_id * comp_
 | `Missing_finished_slash of dir_id * comp_ ]
= resolve

;;


(* test ------------------------------------------------------------ *)

#require "extunix";;

let realpath = ExtUnixAll.realpath

(* where we run the tests *)
let root = realpath "./test"

(* test with a filesystem setup as follows (at "root"):

/a/b.txt -> ../c.txt
/a/d_link -> ../d
/c.txt
/d/e.txt

*)
;;


let fs_ops = {
  root = Private_dir_id root;
  resolve_name = (
    fun did c ->
      let open Unix in
      did |> function (Private_dir_id did) ->
      let path = (did^"/"^c) in
        match lstat path with
        | exception _ -> 
          (* assume not present *)
          Missing
        | stats ->
          match stats.st_kind with
          | S_REG -> File (Private_file_id path)
          | S_DIR -> 
            (* FIXME we need to canonicalize paths, but OCaml lacks a
               native way to do this *)
            Dir (Private_dir_id (realpath path)) 
          | S_LNK -> Sym (readlink path)
          | _ -> failwith "unrecognized type")
}

let cwd = Private_dir_id root

let _ = Unix.chdir root

let resolve = resolve_butlast ~fs_ops ~cwd

;;
let r1 = resolve "/a/b.txt";;
let r2 = resolve "/a/d_link";;
let r3 = resolve "/a/d_link/";;
let r4 = resolve "/a/d_link/e.txt";;
let r5 = resolve "/a/d_link/e.txt/";;

let _ = 
  assert (r1 = `Finished_no_slash ("b.txt",Private_dir_id (root^"/a")));
  assert (r2 = `Finished_no_slash ("d_link", Private_dir_id (root^"/a")));
  assert (r3 = `Finished_slash ("d_link", Private_dir_id (root^"/a")));
  assert (r4 = `Finished_no_slash ("e.txt", Private_dir_id (root^"/d")));
  assert (r5 = `Finished_slash ("e.txt", Private_dir_id (root^"/d")));
  ()
;;
