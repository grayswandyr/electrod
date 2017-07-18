
{ (* BEGIN HEADER *)
  
open Lexing
open SMV_trace_tokens
     
} (* END HEADER *)


let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ]
                 
let digit = [ '0'-'9' ]

let positive = ([ '1'-'9'] digit*)

let number = (digit | positive)
                           
let letter = [ 'A'-'Z' 'a'-'z' ]

let dollar = '$'

let plain_id = dollar? letter (letter | digit | '_' | '#')*

let ident = plain_id (dollar number)?


let loop_msg = "-- Loop starts here"

let state = "->" whitespace+ "State:" whitespace+ digit '.' digit whitespace+ "<-"

let rel_sep = '$'

let ident_sep = '-'

let tuple = (ident (ident_sep ident)*)
            
let valu = ident rel_sep tuple 

rule main split_atomic = parse
  | newline
      { new_line lexbuf; main split_atomic lexbuf }
  | whitespace+
    { main split_atomic lexbuf }

  | loop_msg
      { LOOP }

  | state
      { STATE }

  | "FALSE"
      { FALSE }

  | "TRUE"
      { TRUE }

  | valu as v
    { ATOMIC (split_atomic v) }

  | '='
      { EQUAL }

  | eof 
      { EOF } 
  | _ as c
    { Msg.Fatal.lexical
      @@ fun args -> args None lexbuf
                       ("SMV trace scanning: unexpected character(s): " 
                        ^ (String.make 1 c)) }
    (* and comment openingp tokens = parse *)



    { (* BEGIN FOOTER *)


    } (* END FOOTER *)
