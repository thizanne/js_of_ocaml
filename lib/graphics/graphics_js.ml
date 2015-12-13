(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

include Graphics

type context
let _ = Callback.register_exception "Graphics.Graphic_failure" (Graphic_failure "")

let get_context () =
  Js.Unsafe.(fun_call (variable "caml_gr_state_get") [| |])

let set_context ctx =
  Js.Unsafe.(fun_call (variable "caml_gr_state_set") [| inject ctx |])

let create_context canvas w h =
  Js.Unsafe.(fun_call (variable "caml_gr_state_create")
               [| inject canvas; inject w; inject h|])

let open_canvas x =
  let ctx = create_context x x##width x##height in
  set_context ctx
