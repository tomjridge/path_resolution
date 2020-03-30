(** Path resolution; safe to open *)

module String_util = String_util
module Intf = Intf
open Intf

type follow_last_symlink = Intf.follow_last_symlink

type ('file_id,'dir_id)resolved_path_or_err = ('file_id,'dir_id)Intf.resolved_path_or_err

module Path_resolution = Path_resolution

let resolve : 
monad_ops           : 't monad_ops ->
fs_ops              : ('file_id,'dir_id,'t) fs_ops ->
follow_last_symlink : follow_last_symlink ->
cwd                 : 'dir_id ->
string ->
( ('file_id,'dir_id) resolved_path_or_err, 't) m
= Path_resolution.resolve
(** {[
monad_ops           : 't monad_ops ->
fs_ops              : ('file_id,'dir_id,'t) fs_ops ->
follow_last_symlink : follow_last_symlink ->
cwd                 : 'dir_id ->
string ->
( ('file_id,'dir_id) resolved_path_or_err, 't) m
]} *)

