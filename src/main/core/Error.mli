open Lexing

(** A loc is a pair of a start position and an end position. *)
type loc = position * position

(** 
  A place can be a union of various files and positions
  A place can concret -> referer to an existing file
              abstract -> fname is just a description (e.g. a compilation pass)
*)
type place = loc list


(* Exceptions *)
exception SyntaxError of place 
exception DeadbranchError of string 
exception PlacedDeadbranchError of place * string 

val  error_of_syntax_error : exn -> unit

(** [forge_place filename startpos endpos] forge a place in file [filename] descring area between [startpos] and [endpos] *)
val forge_place : string ->int -> int -> place               

(** [set_filename lexbuf filename] updates [lexbuf] to record the
   fact that the current file name is [filename]. This file name
   is later used in error messages. *)

val set_filename: lexbuf -> string -> unit

(** [place lexbuf] produces a pair of the current token's start and
   end positions. This function is useful when reporting an error
   during lexing. *)

val place: lexbuf -> place

(** display the portion located at [place]*)
val show : ?display_line:bool -> place -> string

(** [error place format ...] displays an error message and exits.
   The error message is located at [place]. The error message
   is composed based on [format] and the extra arguments [...]. *)

val perror: place -> ('a, Format.formatter, unit, unit, unit, 'b) format6  -> 'a
val error: ('a, Format.formatter, unit, unit, unit, 'b) format6  -> 'a
val plog_warning: Easy_logging.Logging.logger -> place -> ('a, unit, string, unit) format4  -> 'a 

(** [pp_place formatter place] prints a place. It is used by
   [@@deriving show] for data structures that contain places.
   As of now, it prints nothing. *)

val pp_place: Format.formatter -> place -> unit

val pp_list: string ->
   (Format.formatter -> 'a -> unit) ->
   Format.formatter -> 'a list -> unit 

val show_list: string ->
   (Format.formatter -> 'a -> unit) ->
   'a list -> string 