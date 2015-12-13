open Js
open XmlHttpRequest

exception Wrong_headers of (int * (string -> string option))
(** The exception raise by perform functions when the check_headers
    parameter returned false. The parameter of the exception is a
    function is like the [headers] function of [http_frame] *)

val perform_raw :
    ?headers:(string * string) list
  -> ?content_type:string
  -> ?post_args:((string * Form.form_elt) list)
  -> ?get_args:((string * string) list)  (* [] *)
  -> ?form_arg:Form.form_contents
  -> ?check_headers:(int -> (string -> string option) -> bool)
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?override_mime_type:string
  -> ?override_method:[< `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ]
  -> response_type:('a response)
  -> string
  -> 'a generic_http_frame Lwt.t
  (** [perform_raw] is the same as {!perform_raw_url} except that an additional
      response_type argument can be given to set the XMLHttpRequest
      responseType, and hence return different types of data for GET
      requests. *)

val perform_raw_url :
    ?headers:(string * string) list
  -> ?content_type:string
  -> ?post_args:((string * Form.form_elt) list)
  -> ?get_args:((string * string) list)  (* [] *)
  -> ?form_arg:Form.form_contents
  -> ?check_headers:(int -> (string -> string option) -> bool)
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?override_mime_type:string
  -> ?override_method:[< `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ]
  -> string
  -> http_frame Lwt.t
  (** [perform_raw_url] makes an asynchronous request to the specified [url] with
      specified options. The result is a cancelable thread returning
      an HTTP frame. By default, if [post_args] and [form_arg] are [None], a GET
      request is used. If [post_args] or [form_arg] is [Some _] (even [Some []]) then a POST
      request is made. But if [override_method] is set, the request method is forced,
      no matter the [post_args] or [form_arg] value. For example, with [override_method]
      set to [`PUT] and [form_arg] set to [Some _] a PUT request including the form data
      will be made. The [check_headers] argument is run as soon as the answer
      code and headers are available. If it returns false, the request is canceled
      and the functions raise the [Wrong_headers] exception *)

val perform :
    ?headers:(string * string) list
  -> ?content_type:string
  -> ?post_args:((string * Form.form_elt) list)
  -> ?get_args:((string * string) list)  (* [] *)
  -> ?form_arg:Form.form_contents
  -> ?check_headers:(int -> (string -> string option) -> bool)
  -> ?progress:(int -> int -> unit)
  -> ?upload_progress:(int -> int -> unit)
  -> ?override_mime_type:string
  -> ?override_method:[< `GET | `POST | `HEAD | `PUT | `DELETE | `OPTIONS | `PATCH ]
  -> Url.url
  -> http_frame Lwt.t
  (** [perform] is the same as {!perform_raw_url} except that the Url argument has type
      {!Url.url}. *)

val get : string -> http_frame Lwt.t
  (** [get url] makes an asynchronous request to the specified [url] *)
