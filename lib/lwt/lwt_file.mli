open Js
open File

val readAsBinaryString : #blob t -> js_string t Lwt.t
val readAsText : #blob t -> js_string t Lwt.t
val readAsText_withEncoding  : #blob t -> js_string t -> js_string t Lwt.t
val readAsDataURL : #blob t -> js_string t Lwt.t
