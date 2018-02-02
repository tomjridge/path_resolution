(* the main entry point for the lib *)

module String_util = String_util

include Path_component
include Fs_ops

include Path_resolution

let resolve = Path_resolution.resolve

