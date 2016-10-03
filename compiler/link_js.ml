
let sourceMappingURL = "//# sourceMappingURL="
let sourceMappingURL_base64 = "data:application/json;base64,"

let drop_prefix ~prefix s =
  let plen = String.length prefix in
  if plen > String.length s
  then None
  else begin
    try
      for i = 0 to String.length prefix - 1 do
        if String.get s i <> String.get prefix i
        then raise Exit
      done;
      Some (String.sub s plen (String.length s - plen))
    with Exit -> None
  end

let _ = drop_prefix ~prefix:"qwe:" "qwe"

let kind line =
  let s =
    match drop_prefix ~prefix:sourceMappingURL_base64 line with
    | Some base64 ->
      Some (B64.decode base64)
    | None ->
      match drop_prefix ~prefix:sourceMappingURL line with
      | Some url ->
        let ic = open_in url in
        let l = in_channel_length ic in
        Some (really_input_string ic l)
      | None -> None
  in
  match s with
  | None -> None
  | Some s -> Some (Json.of_string s)
;;

let link ~output ~files =
  let oc = open_out output in
  let sm = ref [] in
  let line_offset = ref 0 in
  List.iter (fun file ->
    let ic = open_in file in
    (try
       let start_line = !line_offset in
       while true do
         let line = input_line ic in
         match kind line with
         | None ->
           output_string oc line;
           output_string oc "\n";
           incr line_offset
         | Some x ->
           let x = Source_map.of_json x in
           sm := (!line_offset - start_line, x) :: !sm
       done;
     with End_of_file -> ());
    close_in ic;
    output_string oc "\n";
    incr line_offset
  ) files;
  let sm = Source_map.merge !sm in
  let buf = Buffer.create 1024 in
  let pp = Pretty_print.to_buffer buf in
  let json = Source_map.json sm in
  Json.pp pp json;
  let data = Buffer.contents buf in
  let s = sourceMappingURL_base64 ^ (B64.encode data) in
  output_string oc s;
  close_out oc
