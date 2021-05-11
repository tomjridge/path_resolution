(** Path resolution; don't open - comp_ is alised to string *)

module String_util = String_util

module Intf = Intf
include Intf.Public

module Path_resolution = Path_resolution

type ('fid,'did,'sid,'t) resolve_t = 
  follow_last_symlink:follow_last_symlink ->
  cwd:'did ->
  comp_ ->
  ( ('fid,'did,'sid)resolved_path_or_err, 't) m
(** 
Core type of the resolve function
{[
  follow_last_symlink:follow_last_symlink ->
  cwd:'did ->
  comp_ ->
  ( ('fid,'did,'sid)resolved_path_or_err, 't) m
]} *)

let resolve : 
  monad_ops:'t monad_ops ->
  fs_ops:('fid, 'did, 'sid, 't) fs_ops ->
  ('fid,'did,'sid,'t)resolve_t
  = Path_resolution.resolve
(** 
The resolve function provided by this library, parameterized by
the monad and the underlying filesystem operations

{[
  monad_ops:'t monad_ops ->
  fs_ops:('fid, 'did, 'sid, 't) fs_ops ->
  ('fid,'did,'sid,'t)resolve_t
]} *)

