(** Contains pretty-printing facilities. Note: same licence as Fmt. *)

(** Licence: ISC *)

(** Straight extension of Fmt to avoid having to open Fmt and Fmtc *)
include Fmt

let nbsp = const string " "

let hardline = Format.pp_force_newline

let lbrace = const string "{"
let rbrace = const string "}"

let lparen = const string "("
let rparen = const string ")"

let langle = const string "<"
let rangle = const string ">"

let lbracket = const string "["
let rbracket = const string "]"

let squote = const string "'"
let dquote = const string "\""

let bquote = const string "`"

let semi = const string ";"

let colon = const string ":"

let comma = const string ","

let dot = const string "."

let sharp = const string "#"

let slash = const string "/"

let backslash = const string "\\"

let equal = const string "="

let qmark = const string "?"

let tilde = const string "~"

let at = const string "@"

let percent = const string "%"

let dollar = const string "$"

let caret = const string "^"

let ampersand = const string "&"

let star = const string "*"

let plus = const string "+"

let minus = const string "-"

let underscore = const string "_"

let bang = const string "!"

let bar = const string "|"

let squotes ppf v = quote ~mark:"'" ppf v

let bquotes ppf v = quote ~mark:"`" ppf v

let angles pp_v ppf v = pf ppf "@[<1><%a>@]" pp_v v

(* all **... operators associate to the RIGHT and are of stronger precedence
   than @@ but lower than function application *)
let ( **@ ) p1 p2 = append p1 p2

let ( **< ) p1 p2 = prefix p1 p2
                     
let ( **> ) p1 p2 = suffix p1 p2

(* append, with a break *hint* in the middle *)
let ( **! ) p1 p2 = pair p1 p2

(* append, with a space or a break in the middle *)
let ( **- ) p1 p2 = pair ~sep:sp p1 p2

(* creates a box and surrounds it by delimiters bef and aft *)
let surround bef aft pp_v out v =
  bef out ();
  pp_v out v;
  aft out ()

(* like surround, with a non-breakable-space right after bef (resp. right before aft) *)
let surround_ bef aft pp_v out v =
  bef out ();
  pf out " ";
  pp_v out v;
  pf out " ";
  aft out ()

let braces_ ppv_v out v =
  surround_ lbrace rbrace ppv_v out v

let brackets_ ppv_v out v =
  surround_ lbracket rbracket ppv_v out v

let parens_ ppv_v out v =
  surround_ lparen rparen ppv_v out v

let angles_ ppv_v out v =
  surround_ langle rangle ppv_v out v



let repeat n pp =
  assert (n >= 0);
  let rec walk n pp =
    if n = 0 then nop
    else pp **< walk (n - 1) pp
  in walk n pp


let infix ?(indent = 0) middle left right out (l, r) =
  Format.pp_open_hovbox out indent;
  Format.pp_open_hovbox out indent;
  left out l;
  sp out ();
  middle out ();
  Format.pp_close_box out ();
  sp out ();
  right out r;
  Format.pp_close_box out ()
  
let tuple2 = pair

let tuple3 ?sep1:(pp_sep1 = cut) ?sep2:(pp_sep2 = cut)
      pp1 pp2 pp3 ppf (x1, x2, x3) =
  pp1 ppf x1;
  pp_sep1 ppf ();
  pp2 ppf x2;
  pp_sep2 ppf ();
  pp3 ppf x3


let box2 out v = box ~indent:2 out v
let hbox2 out v = hbox out v
let vbox2 out v = vbox ~indent:2 out v
let hvbox2 out v = hvbox ~indent:2 out v
