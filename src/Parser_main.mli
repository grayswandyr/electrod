(** Calls the parser and returns the raw AST. *)

(** [parse infile] parses the file [infile] and returns its raw AST. *)
val parse_file : string -> Raw.raw_problem

(** parses a string and returns its corresponding raw AST*)
val parse_string : string -> Raw.raw_problem
