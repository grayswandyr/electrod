(*******************************************************************************
 * Time-stamp: <2017-11-15 CET 15:50:19 David Chemouil>
 * 
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2017 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSES/MPL-2.0.txt
 ******************************************************************************)

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

let plain_id = ('_' dollar)? letter (letter | digit | '_' | '#')*

let ident = plain_id (dollar number)?


let loop_msg = "-- Loop starts here"

let state = "->" whitespace+ "State:" whitespace+ digit '.' digit whitespace+ "<-"

let rel_sep = '-'

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
    { match split_atomic v with
      | None ->
          Msg.Fatal.lexical
          @@ fun args -> args None lexbuf
                       ("SMV trace scanning: unknown variable: " ^ v)                           
      | Some pair -> ATOMIC pair
    }

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
