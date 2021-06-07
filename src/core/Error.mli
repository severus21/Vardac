open Lexing

(* A place is a pair of a start position and an end position. *)

type loc = position * position
type place = loc list


(* Exceptions *)
exception SyntaxError of place 
exception DeadbranchError of string 

val  error_of_syntax_error : exn -> unit

(* [forge_place filename startpos endpos] forge a place in file [filename] descring area between [startpos] and [endpos] *)
val forge_place : string ->int -> int -> place               

(* [set_filename lexbuf filename] updates [lexbuf] to record the
   fact that the current file name is [filename]. This file name
   is later used in error messages. *)

val set_filename: lexbuf -> string -> unit

(* [place lexbuf] produces a pair of the current token's start and
   end positions. This function is useful when reporting an error
   during lexing. *)

val place: lexbuf -> place

(* display the portion located at [place]*)
val show : place -> string

(* [error place format ...] displays an error message and exits.
   The error message is located at [place]. The error message
   is composed based on [format] and the extra arguments [...]. *)

val error: place -> ('a, out_channel, unit, 'b) format4 -> 'a

(* [pp_place formatter place] prints a place. It is used by
   [@@deriving show] for data structures that contain places.
   As of now, it prints nothing. *)

val pp_place: Format.formatter -> place -> unit
