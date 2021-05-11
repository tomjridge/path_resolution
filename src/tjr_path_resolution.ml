(** Path resolution; safe to open *)

module String_util = String_util
module Intf = Intf
open Intf

type follow_last_symlink = Intf.follow_last_symlink

type ('fid,'did,'sid)resolved_path_or_err = ('fid,'did,'sid)Intf.resolved_path_or_err

module Path_resolution = Path_resolution

let resolve : 
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
  = Path_resolution.resolve
(** {[
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
]} *)

