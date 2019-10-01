(* the main entry point for the lib *)

module String_util = String_util
module Fs_ops = Fs_ops

include Path_component
include Fs_ops

include Path_resolution

let resolve = Path_resolution.resolve

