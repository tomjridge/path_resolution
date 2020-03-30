(* test ------------------------------------------------------------ *)

open Tjr_path_resolution
open Tjr_path_resolution.Intf

type file_id = Private_file_id of string
type dir_id = Private_dir_id of string

let realpath = ExtUnixAll.realpath

let old_cwd = Unix.getcwd()

(* where we run the tests; note this assumes we start in the directory containing root *)
let root = realpath "."

let _ = Unix.chdir root


(* test with a filesystem setup as follows (at "root"):

/a/b.txt -> ../c.txt
/a/d_link -> ../d
/c.txt
/d/e.txt

*)
;;


let monad_ops = imperative_monad_ops
open Tjr_monad.Imperative

let fs_ops = {
  root = Private_dir_id root;
  resolve_comp = (
    fun did c -> monad_ops.return @@
      let open Unix in
      did |> function (Private_dir_id did) ->
      let path = (did^"/"^c) in
        match lstat path with
        | exception _ -> 
          (* assume not present *)
          RC_missing
        | stats ->
          match stats.st_kind with
          | S_REG -> RC_file (Private_file_id path)
          | S_DIR -> 
            (* FIXME we need to canonicalize paths, but OCaml lacks a
               native way to do this *)
            RC_dir (Private_dir_id (realpath path)) 
          | S_LNK -> RC_sym (readlink path)
          | _ -> failwith "unrecognized type")
}

let cwd = Private_dir_id root

let resolve s = Path_resolution.resolve_butlast ~monad_ops ~fs_ops ~cwd s |> Imperative.of_m

;;
let r1 = resolve "/a/b.txt";;
let r2 = resolve "/a/d_link";;
let r3 = resolve "/a/d_link/";;
let r4 = resolve "/a/d_link/e.txt";;
let r5 = resolve "/a/d_link/e.txt/";;
(* let r6 = resolve "/";; *)

let _ = 
  assert (r1 = `Finished_no_slash ("b.txt",Private_dir_id (root^"/a")));
  assert (r2 = `Finished_no_slash ("d_link", Private_dir_id (root^"/a")));
  assert (r3 = `Finished_slash ("d_link", Private_dir_id (root^"/a")));
  assert (r4 = `Finished_no_slash ("e.txt", Private_dir_id (root^"/d")));
  assert (r5 = `Finished_slash ("e.txt", Private_dir_id (root^"/d")));
  ()
;;

(* resolve' --------------------------------------------------------- *)

let resolve s = 
  Path_resolution.resolve' ~monad_ops ~fs_ops ~follow_last_symlink:`Always ~cwd s |> of_m

let r1 = resolve "/"

let root_id = Private_dir_id root

let _ = 
  assert (r1 = `Finished_root)


(* resolve_simplified ----------------------------------------------- *)

let resolve s = 
  Path_resolution.resolve_simplified ~monad_ops ~fs_ops ~follow_last_symlink:`Always ~cwd s |> of_m

let dest_Ok = function
  | Ok x -> x
  | _ -> failwith __LOC__

let r1 = dest_Ok @@ resolve "/a/b.txt";;
let r2 = dest_Ok @@ resolve "/a/d_link";;
let r3 = dest_Ok @@ resolve "/a/d_link/";;
let r4 = dest_Ok @@ resolve "/a/d_link/e.txt";;
let r5 = dest_Ok @@ resolve "/a/d_link/e.txt/";;
let r6 = dest_Ok @@ resolve "/";;

let _ = 
  assert (r1.result = File (Private_file_id (root^"/c.txt")));
  assert (r2.result = Dir (Private_dir_id (root^"/d")));
  assert (r3.result = Dir (Private_dir_id (root^"/d")));
  assert (r3.trailing_slash);
  assert (r4.result = File(Private_file_id (root^"/d/e.txt")));
  assert (r5.result = File(Private_file_id (root^"/d/e.txt")));
  assert (r5.trailing_slash);
  ()
;;





(* finally, reset cwd ------------------------------------------------ *)

let _ = Unix.chdir old_cwd

;;

let _ = print_endline "Tests passed"
