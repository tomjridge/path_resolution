(** Path resolution algorithm *)

(* NOTE root is special! Not to be confused with '/' appearing inside
   a path *)

(** 

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

TODO:

- add MAX_SYMLINK and MAX_PATH

- construct a unix/windows path compatibility library based on this
  code

- add testing and code coverage

- test this code against SibylFS model and real-world traces; replace
  SibylFS model with this one

*)

open Intf
open String_util


(* whether the state is "finished"; if is_absolute, then we can do
   more (ie we are not finished); otherwise if path is empty, then we
   are finished *)
let is_finished s = 
  not (s.is_absolute) && (s.path = "")


(* 

When we encounter a symlink, we need to get the symlink contents (a
string) and decide what to do next.

If the symlink starts with a slash, then we need to resolve an
absolute path, otherwise a relative path.

Essentially we then join the contents of the symlink with the
remainder of the path we are currently processing.

*)
let state_from_symlink ~cwd ~symlink_contents ~path = 
  let str = symlink_contents in
  match strip_leading_slash str with
  | Some str' -> 
    { cwd;
      is_absolute=true; 
      path=(str'^"/"^path) }
  | None -> 
    { cwd;
      is_absolute=false;
      path=str^"/"^path }



(* Get the next component of the path. This function deals with strings, not state *)
let get_next_comp (s:_ state) = 
  assert (not (is_finished s));
  match s.is_absolute with
  | true -> 
    {s with is_absolute=false} |> fun s ->
    if is_finished s 
    then `Root_finished,s
    else `Root_unfinished,s

  | false ->
    (* we know that path <> "" *)
    assert(s.path <> "");
    split_on_first_slash s.path |> fun (comp,rest) ->
    match rest with 
    | None -> `Finished_no_slash comp, {s with path=""}
    | Some "" -> `Finished_slash comp, {s with path=""}
    | Some rest -> `Comp_with_slash comp, {s with path=rest}

let _ = get_next_comp


(* Step the path resolution state. This function interacts with the
   fs. *)
let step ~monad_ops:{return;bind} ~fs_ops s =
  let ( >>= ) = bind in
  (* get the next path component *)
  s |> get_next_comp |> function

  | `Root_finished,_s -> return @@ `Finished_root
  | `Root_unfinished,s -> return @@ `Ok {s with cwd=fs_ops.root}

  | `Finished_no_slash c,s -> 
    (* No more path components left. It may be that the current
       component refers to a symlink, in which case we need to do more
       processing. This is dealt with elsewhere. *)
    return @@ `Finished_no_slash (c,s.cwd)  

  | `Finished_slash c,s -> 
    (* Again, c may be a symlink *)
    return @@ `Finished_slash (c,s.cwd)

  | `Comp_with_slash c,s -> 
    (* 

We have not finished, so there are more components to process. 

These may just be extra trailing slashes. In addition, the current
entry may be missing. An example where both happen is: /a/b//// where
b is a missing target of a dir rename. This case is likely acceptable.

On the other hand we may have a path /a/b/x/y/z, where again b is
missing. This is likely an error.

NOTE there is the question where "" should be silently ignored, or
actually passed to fs_ops to resolve

    *)
    fs_ops.resolve_comp s.cwd c >>= function
    | RC_file fid -> 
      (* NOTE we have something like f.txt/// or f.txt/a/b; f.txt/ is
         actually treated as valid on some platforms, especially if
         f.txt is a symlink to a file, so that the trailing slash
         means "follow symlink" FIXME add a flag for this *)
      return @@ `Error (`File_followed_by_slash_etc (s,c,fid))

    | RC_dir d -> return @@ `Ok {s with cwd=d}

    | RC_sym (_sid,str) -> (
        (* We have something like symlink// or symlink/x/y/z; in the
           first case, we force the follow of the symlink FIXME some
           platforms do not do this, so parameterize *)
        return @@ `Ok (state_from_symlink ~cwd:s.cwd ~symlink_contents:str ~path:s.path))
    | RC_missing -> 
      (* NOTE missing/ is ok if the target is a missing directory *)
      (* This may or may not be valid. So return all that we know at
         this point; path may be empty, or eg "///" so this could be a
         valid path *)
      return @@ `Missing_slash(c,s)

let _ : 
  monad_ops:'a monad_ops ->fs_ops:('b, 'c, 'd, 'a) fs_ops ->
  'c state ->
  ([> `Error of
        [> `File_followed_by_slash_etc of 'c state * comp_ * 'b ]
   | `Finished_root
   | `Finished_no_slash of comp_ * 'c  (* maybe symlink *)
   | `Finished_slash of comp_ * 'c  (* maybe symlink *)
   | `Missing_slash of comp_ * 'c state
   | `Ok of 'c state ],
   'a)
    m
  = step


(** Resolve all but the last component; not used by the main `resolve`
    function *)
let resolve_butlast ~monad_ops ~fs_ops ~cwd = 
  let {return;bind} = monad_ops in
  let ( >>= ) = bind in
  let step = step ~monad_ops ~fs_ops in
  let rec f s = 
    assert (not (is_finished s));
    step s >>= function
    | `Ok s -> f s
    | `Finished_root       -> return @@ `Finished_root
    | `Finished_no_slash x -> return @@ `Finished_no_slash x
    | `Finished_slash x    -> return @@ `Finished_slash x
    | `Error e             -> return @@ `Error e
    | `Missing_slash x     -> return @@ `Missing_slash x
  in  
  fun s ->
    match strip_leading_slash s with
    | None -> f { cwd; is_absolute=false; path=s }
    | Some path -> f { cwd; is_absolute=true; path }


(* NOTE that the last component is not resolved; follow_last_symlink
   etc can be done as another step *)
let _ : 
  monad_ops: 't monad_ops ->
  fs_ops: ('fid,'did,'sid,'t) fs_ops -> cwd:'did -> string ->
  ([> `Error of [> `File_followed_by_slash_etc of 'did state * comp_ * 'fid ]
   | `Finished_root
   | `Finished_no_slash of comp_ * 'did
   | `Finished_slash of comp_ * 'did
   | `Missing_slash of comp_ * 'did state ],
   't)m
  = resolve_butlast
    
;;

(* 

NOTE `resolve_butlast` is cumbersome to use if you want information
about the last component, and want to deal with `follow_last_symlink`
(you have to call `resolve_butlast` repeatedly, possibly in
conjunction with calling fs_ops.resolve_comp).

The next piece of code `resolve` returns info on the last component,
but NOTE there are several ways implementations commonly differ in
behaviour

*)



let resolve' ~monad_ops ~fs_ops ~(follow_last_symlink:follow_last_symlink) ~cwd = 
  let { return; bind } = monad_ops in
  let ( >>= ) = bind in
  let step = step ~monad_ops ~fs_ops in
  let rec f s = 
    assert (not (is_finished s));
    step s >>= function
    | `Ok s -> f s

    | `Finished_root -> return `Finished_root

    | `Finished_no_slash (comp,dir) -> (
        (* Cases where there is no trailing slash *)
        fs_ops.resolve_comp dir comp >>= function
        | RC_file fid -> return @@ `Finished_no_slash_file (dir,comp,fid)
        | RC_dir d -> return @@ `Finished_no_slash_dir (dir,comp,d)
        | RC_sym (sid,str) -> (
            (* A symlink *without* a trailing slash *)
            match follow_last_symlink with
            | `Always ->
              state_from_symlink ~cwd:dir ~symlink_contents:str ~path:"" |> f
            | _ -> 
              return @@ `Finished_no_slash_symlink (dir,comp,sid,str))
        | RC_missing -> return @@ `Missing_finished_no_slash(dir,comp))

    | `Finished_slash (comp,dir) -> (
        (* Cases where there is a trailing slash *)
        fs_ops.resolve_comp dir comp >>= function
        | RC_file fid -> 
          (* NOTE potentially an error *)
          return @@ `Finished_slash_file (dir,comp,fid)

        | RC_dir d -> 
          (* Never (?) an error *)
          return @@ `Finished_slash_dir (dir,comp,d)

        | RC_sym (_sid,str) -> (
            (* NOTE there is a trailing slash; FIXME the following
               line doesn't hold on all platforms; parameterize *)
            match follow_last_symlink with
            | `Always | `If_trailing_slash -> 
              state_from_symlink ~cwd:dir ~symlink_contents:str ~path:"" |> f
            | `Never -> 
              (* NOTE this case only occurs if the follow_last_symlink
                 is false - which may occur on some platforms FIXME *)
              (* `Finished_slash_symlink (dir,comp,str) *)
              failwith "impossible at the moment"  (* FIXME? *))

        | RC_missing -> return @@ `Missing_finished_slash(dir,comp))

    | `Error e -> return @@ `Error e

    | `Missing_slash (comp,s) -> 
      (* NOTE that there may well be further stuff to resolve in
         s.path, likely indicating an error; this is dealt with
         later *)
      return @@ `Missing_slash (comp,s)
  in  
  fun s ->
    match strip_leading_slash s with
    | None -> f { cwd; is_absolute=false; path=s }
    | Some path -> f { cwd; is_absolute=true; path }

let _ : 
  monad_ops:'a monad_ops ->
  fs_ops:('b, 'c, 'd, 'a) fs_ops ->
  follow_last_symlink:follow_last_symlink ->
  cwd:'c ->
  comp_ ->
  ([> `Error of [> `File_followed_by_slash_etc of 'c state * comp_ * 'b ]
   | `Finished_no_slash_dir of 'c * comp_ * 'c
   | `Finished_no_slash_file of 'c * comp_ * 'b
   | `Finished_no_slash_symlink of 'c * comp_ * 'd * comp_
   | `Finished_root
   | `Finished_slash_dir of 'c * comp_ * 'c
   | `Finished_slash_file of 'c * comp_ * 'b
   | `Missing_finished_no_slash of 'c * comp_
   | `Missing_finished_slash of 'c * comp_
   | `Missing_slash of comp_ * 'c state ],
   'a)
    m
  = resolve'


(* the above has a lot of cases; we pick out some commonalities to
   simplify subsequent case splitting *)

(** a "special" result for root *)
(* FIXME there is a question here about what to do exactly for root;
   hopefully the following will suffice *)
let simplified_result_root ~fs_ops =
  let root = fs_ops.root in
  { parent_id=root; comp=""; result=(Dir root); trailing_slash=false}

let resolve_simplified ~monad_ops ~fs_ops ~(follow_last_symlink:follow_last_symlink) ~cwd s = 
  let { return;bind } = monad_ops in
  let ( >>= ) = bind in
  resolve' ~monad_ops ~fs_ops ~follow_last_symlink ~cwd s >>= function
  | `Error e -> return @@ Error e

  | `Finished_root -> return @@ Ok (simplified_result_root ~fs_ops)

  | `Finished_no_slash_dir (parent_id,comp,did) -> 
    return @@ Ok { parent_id; comp; result=(Dir did); trailing_slash=false }

  | `Finished_no_slash_file (parent_id,comp,fid) -> 
    return @@ Ok {parent_id; comp; result=(File fid); trailing_slash=false }       

  | `Finished_no_slash_symlink (parent_id,comp,sid,str) -> 
    return @@ Ok {parent_id; comp; result=(Sym (sid,str)); trailing_slash=false }       

  | `Finished_slash_dir (parent_id,comp,did) -> 
    return @@ Ok { parent_id; comp; result=(Dir did); trailing_slash=true }

  | `Finished_slash_file (parent_id,comp,fid) -> 
    (* NOTE this is often an error FIXME include a further layer to
       capture these cases? *)
    return @@ Ok { parent_id; comp; result=(File fid); trailing_slash=true }

  | `Finished_slash_symlink (parent_id,comp,sid,str) -> 
    (* NOTE the trailing slash typically forces the symlink to be
       followed *)
    return @@ Ok { parent_id; comp; result=(Sym (sid,str)); trailing_slash=true }

  | `Missing_slash (comp,s) -> (
     (*

We have a missing component followed by further stuff in s.path. 

We distinguish between the cases where there are trailing slashes only
in s.path, and where there is something other than a trailing
slash.

NOTE there may be other choices here

*)

      assert(s.is_absolute = false);
      let remaining = s.path in
      (* check if all slashes *)
      match all_slashes remaining with
      | true -> 
        (* NOTE the component is missing, and the unprocessed path suffix
           contains only slashes; typically this would represent a target of
           a rename of a directory, but some platforms allow this for file
           renames etc; we return an `Ok`, but be aware that this may be an
           error with some platforms and some syscalls *)
        return @@ Ok { parent_id=s.cwd; comp; result=Missing; trailing_slash=true }

      | false -> 
        (* This is an error case eg /a/b/missing/some/more/stuff *)
        return @@ Error (`Missing_slash_etc (comp,s.cwd,remaining)))

  | `Missing_finished_no_slash (parent_id,comp) -> 
    return @@ Ok { parent_id; comp; result=Missing; trailing_slash=false }

  | `Missing_finished_slash (parent_id,comp) -> 
    return @@ Ok { parent_id; comp; result=Missing; trailing_slash=false }

;;

let resolve ~(monad_ops:'t monad_ops) ~(fs_ops:('fid,'did,'sid,'t)fs_ops) = 
  resolve_simplified ~monad_ops ~fs_ops

let _ : 
  monad_ops:'t monad_ops ->
  fs_ops:('fid, 'did, 'sid, 't) fs_ops ->
  follow_last_symlink:follow_last_symlink ->
  cwd:'did ->
  comp_ ->
  ((('fid, 'did, 'sid) simplified_result,
    [> `File_followed_by_slash_etc of 'did state * comp_ * 'fid
    | `Missing_slash_etc of comp_ * 'did * comp_ ])
     result, 't)
    m  
  = resolve
;;


