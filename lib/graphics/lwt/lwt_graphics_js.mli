(** {6 Mouse and keyboard events} *)
val loop : event list -> (status -> unit) -> unit
(** Loops forever and listen to the given events. Those events automatically
    returns a status record, which is used by the function given in argument. *)

(** {6 Mouse and keyboard polling} *)
val mouse_pos : unit -> (int * int) Lwt.t
(** Return the position of the mouse cursor, relative to the
   graphics window. If the mouse cursor is outside of the graphics
   window, [mouse_pos()] returns a point outside of the range
   [0..size_x()-1, 0..size_y()-1]. *)

val button_down : unit -> bool Lwt.t
(** Return [true] if the mouse button is pressed, [false] otherwise. *)

val read_key : unit -> char Lwt.t
(** Wait for a key to be pressed, and return the corresponding
    character. Keypresses are queued. *)
