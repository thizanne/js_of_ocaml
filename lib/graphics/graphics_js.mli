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

(** Javascript binding for Graphics lib *)

include module type of Graphics

(** {6 Initializations} *)
type context
(** type of a graphic context *)

val open_graph : string -> unit
(** Open a graphics window.
    The graphics window is cleared and the current point is set
    to (0, 0). The string argument is used to pass optional
    information on the desired graphics mode, the graphics window
    size, and so on. Specification can be found at
    http://www.w3schools.com/jsref/met_win_open.asp.
    Note: an extra specification is availble, "target",
    to specifies the target attribute or the name of the window. *)

val open_canvas : Dom_html.canvasElement Js.t -> unit
(** use a canvas to setup the current context *)

val get_context : unit -> context
(** Get the current context *)

val set_context : context -> unit
(** Set the current context *)
