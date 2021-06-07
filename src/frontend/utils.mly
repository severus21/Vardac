%%

(* A term is annotated with its start and end positions, for use in error
   messages. *)

%public %inline placed(X):
  x = X
    { { place = [$loc]; value = x } }
(* -------------------------------------------------------------------------- *)

%public flexible_sequence(X):
| (* nothing *)
    { [] }
| x = X
    { [x] }
| x = X xs = flexible_sequence(X)
    { x :: xs }

(* In a right-flexible list, the last delimiter is optional, i.e., [delim] can
   be viewed as a terminator or a separator, as desired. *)

(* There are several ways of expressing this. One could say it is either a
   separated list or a terminated list; this works if one uses right recursive
   lists. Or, one could say that it is a separated list followed with an
   optional delimiter; this works if one uses a left-recursive list. The
   following formulation is direct and seems most natural. It should lead to
   the smallest possible automaton. *)

%public right_flexible_list(delim, X):
| (* nothing *)
    { [] }
| x = X
    { [x] }
| x = X delim xs = right_flexible_list(delim, X)
    { x :: xs }

%public right_list(delim, X):
| x = X
    { [x] }
| x = X delim xs = right_flexible_list(delim, X)
    { x :: xs }

(* In a left-flexible list, the first delimiter is optional, i.e., [delim] can
   be viewed as an opening or as a separator, as desired. *)

(* Again, there are several ways of expressing this, and again, I suppose the
   following formulation is simplest. It is the mirror image of the above
   definition, so it is naturally left-recursive, this time. *)

%public reverse_left_flexible_list(delim, X):
| (* nothing *)
    { [] }
| x = X
    { [x] }
| xs = reverse_left_flexible_list(delim, X) delim x = X
    { x :: xs }

%public %inline left_flexible_list(delim, X):
  xs = reverse_left_flexible_list(delim, X)
    { List.rev xs }