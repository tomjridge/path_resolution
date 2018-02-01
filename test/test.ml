(* test ------------------------------------------------------------ *)

open Path_resolution

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


let fs_ops = Fs_ops.{
  root = Private_dir_id root;
  resolve_comp = (
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


(* finally, reset cwd *)
let _ = Unix.chdir old_cwd

;;

let _ = print_endline "Tests passed"