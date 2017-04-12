

let parse_file infile = 
  CCIO.with_in infile @@
  fun ic ->
  let lexbuf = Lexing.from_channel ic in
  try 
    let raw_univ, raw_decls, raw_goals, raw_assignments =
      Parser.parse_problem (Scanner.main (Some infile)) lexbuf
    in Raw.problem (Some infile) raw_univ raw_decls raw_goals raw_assignments
  with Parser.Error ->
    Msg.Fatal.syntax @@ fun args -> args infile lexbuf

(*$inject   
  let str = "univ : { a a a a$1 .. a$4 a$3 .. a$22 }; sat true"

*)

(*$T parse_string 

  qtest_exn parse_string str 

  try ignore (parse_string "univ : { a a a a$1 .. a$4 a$3 .. a$22 } sat true"); true with Parser.Error -> false
*)
let parse_string s = 
  let lexbuf = Lexing.from_string s in
  let raw_univ, raw_decls, raw_goals, raw_assignments =
    Parser.parse_problem (Scanner.main None) lexbuf in
  Raw.problem None raw_univ raw_decls raw_goals raw_assignments


