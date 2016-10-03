(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

type map = {
  gen_line : int;
  gen_col : int;
  ori_source : int;
  ori_line : int;
  ori_col : int;
  ori_name : int option
}

type mapping = map list

type t = {
  version : int;
  file : string;
  sourceroot : string option;
  mutable sources : string list;
  mutable sources_content : string option list option;
  mutable names : string list;
  mutable mappings : mapping ;
}

let string_of_mapping mapping =
  let a = Array.of_list mapping in
  let len = Array.length a in
  Array.stable_sort (fun t1 t2 ->
    match compare t1.gen_line t2.gen_line with
      | 0 -> compare t1.gen_col t2.gen_col
      | n -> n) a;
  let buf = Buffer.create 1024 in

  let gen_line = ref 0 in
  let gen_col = ref 0 in
  let ori_source = ref 0 in
  let ori_line = ref 0 in
  let ori_col = ref 0 in
  let ori_name = ref 0 in

  let rec loop prev i =
    if i < len then
      let c = a.(i) in
      if
        prev >= 0 &&
        c.ori_source = a.(prev).ori_source &&
        c.ori_line = a.(prev).ori_line &&
        c.ori_col = a.(prev).ori_col
      then
        (* We already are at this location *)
        loop prev (i + 1)
      else if
        i + 1 < len &&
        c.gen_line = a.(i+1).gen_line && c.gen_col = a.(i+1).gen_col
      then
        (* Only keep one source location per generated location *)
        loop prev (i + 1)
      else begin
        if !gen_line <> c.gen_line then begin
          assert (!gen_line < c.gen_line);
          for _i = !gen_line to c.gen_line - 1 do
            Buffer.add_char buf ';';
          done;
          gen_col := 0; gen_line := c.gen_line
        end else if i > 0 then
          Buffer.add_char buf ',';
          let l =
            c.gen_col - !gen_col ::
            if c.ori_source = -1 then
              []
            else
              c.ori_source - !ori_source ::
              c.ori_line - !ori_line ::
              c.ori_col - !ori_col ::
              match c.ori_name with
              | None   -> []
              | Some n -> let n' = !ori_name in ori_name := n; [n - n']
          in
          gen_col := c.gen_col;
          if c.ori_source <> -1 then begin
            ori_source := c.ori_source;
            ori_line := c.ori_line;
            ori_col := c.ori_col
          end;
          Vlq64.encode_l buf l;
          loop i (i + 1)
        end
  in
  loop (-1) 0;
  Buffer.contents buf

let mapping_of_string str =
  let total_len = String.length str in

  let gen_col = ref 0 in
  let ori_source = ref 0 in
  let ori_line = ref 0 in
  let ori_col = ref 0 in
  let ori_name = ref 0 in

  let rec readline line pos acc =
    if pos >= total_len
    then acc
    else
      let last =
        try
          String.index_from str pos ';'
        with Not_found -> total_len
      in
      gen_col := 0;
      let pos, acc = read_tokens line  pos last acc in
      readline (succ line) pos acc
  and read_tokens line start stop acc =
    let last =
      try
        min (String.index_from str start ',') stop
      with Not_found -> stop
    in
    let v = Vlq64.decode_l str ~pos:start ~len:(last - start)  in
    match v with
    | [_] | [_;_;_;_] | [_;_;_;_;_] ->
      let rec loop l v =
        match l, v with
        | [], [] -> ()
        | [], _  -> assert false
        | r::rs, [] -> r:= -1; loop rs v
        | r::rs, v::vs -> r := !r + v; loop rs vs
      in
      loop [gen_col; ori_source; ori_line; ori_col; ori_name] v;
      let acc = { gen_line = line;
                  gen_col  = !gen_col;
                  ori_source = !ori_source;
                  ori_line = !ori_line;
                  ori_col = !ori_col;
                  ori_name = if !ori_name >= 0 then Some !ori_name else None
                } :: acc
      in
      if last = stop
      then (last + 1, acc)
      else read_tokens line (last + 1) stop acc
    | [] -> last + 1 , acc
    | _  -> invalid_arg "Source_map.mapping_of_string"
  in
  readline 0 0 []
;;

(* let map_str = "\
 * ;;;;EAEE,EAAE,EAAC,CAAE;ECQY,UACC"
 * let map = mapping_of_string map_str
 * let map_str' = string_of_mapping map
 * let map' = mapping_of_string map_str'
 * let _ = print_endline map_str
 * let _ = print_endline map_str' *)


let json t =
  `O [
     "version",       `Float  (float_of_int t.version);
     "file",          `String t.file;
     "sourceRoot",    `String (match t.sourceroot with None -> "" | Some s -> s);
     "names",         `A (List.map (fun s -> `String s) t.names);
     "mappings",      `String (string_of_mapping t.mappings);
     "sources",       `A (List.map (fun s -> `String s) t.sources);
     "sourcesContent",`A
		     (match t.sources_content with
		      | None -> []
		      | Some l ->
			 List.map
			   (function
			     | None -> `Null
			     | Some s -> `String s) l);
  ]

let invalid () = invalid_arg "Source_map.of_json"

let string name rest =
  try
    match List.assoc name rest with
    | `String s -> Some s
    | `Null -> None
    | _ -> invalid ()
  with Not_found -> None

let list_string name rest =
  try
    match List.assoc name rest with
    | `A l -> Some (
      List.map (function
        | `String s -> s
        | _ -> invalid ()) l)
    | _ -> invalid ()
  with Not_found -> None

let list_string_opt name rest =
  try
    match List.assoc name rest with
    | `A l -> Some (List.map (function
      | `String s -> Some s
      | `Null     -> None
      | _         -> invalid ()) l)
    | _ -> invalid ()
  with Not_found -> None


let of_json json =
  match json with
  | `O (("version", `Float version)
        :: rest) when int_of_float version = 3 ->
    let def v d =
      match v with None -> d | Some v -> v
    in
    let file = string "file" rest in
    let sourceroot = string "sourceRoot" rest in
    let names = list_string "names" rest in
    let sources = list_string "sources" rest in
    let sources_content = list_string_opt "sourcesContent" rest in
    let mappings = string "mappings" rest in
    { version = int_of_float version;
      file = def file "";
      sourceroot;
      names = def names [];
      sources_content = sources_content;
      sources = def sources [];
      mappings = mapping_of_string (def mappings "");
    }
  | _ -> invalid ()

let count p string =
  let r = ref 0 in
  String.iter (fun c -> if c = p then incr r);
  !r

let merge _ = assert false
(*let merge l =
  match l with
  | []  -> []
  | [x] -> [x]
  | x :: xs ->
    let rec loop gen_line l =
*)
