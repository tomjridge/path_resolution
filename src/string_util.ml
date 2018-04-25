(* strings ---------------------------------------------------------- *)

let slash_char = '/'


let split_on_first_slash s =
  let open String in
  match index s slash_char with
  | exception Not_found -> (s,None)
  | i -> (sub s 0 i, Some(sub s (i+1) (length s - (i+1))))


let first_char s =
  assert(s<>"");
  String.get s 0


let drop_first_char s = 
  assert (s<>"");
  String.sub s 1 (String.length s -1)


let strip_leading_slash str = 
  match str <>"" && first_char str = slash_char with
  | true -> Some (drop_first_char str)
  | false -> None


let starts_with_slash str = strip_leading_slash str <> None


let string_explode s = 
  let r = ref [] in
  String.iter (fun c -> r:=c::!r) s;
  List.rev !r


let all_slashes s = 
  s |> string_explode |> List.for_all (fun c -> c = slash_char)

