
%{
  
module R = Raw

module G = GenGoal  
%}
  
%start <Raw.raw_urelements list
 * Raw.raw_declaration list
 * Raw.raw_goal list> parse_problem

%token UNIV NONE VAR COLON SEMI EOF EQ IN NEQ AND OR HISTORICALLY
%token IMPLIES IFF UNTIL RELEASE SINCE NEXT ONCE PREVIOUS LET
%token LPAREN RPAREN LBRACKET RBRACKET DOTDOT PLUS ARROW
%token ALL SOME DISJ ONE LONE NO COMMA LBRACE RBRACE BAR
%token GT GTE LT LTE TRUE FALSE SOMETIME ALWAYS NOT
%token SAT TILDE HAT STAR IDEN ELSE CONST
%token INTER OVERRIDE LPROJ RPROJ MINUS DOT PRIME
%token THEN NOT_IN
// for integer expressions
%token NEG ADD SUB HASH //INT

%token <string> FBUILTIN        (* for formulas *)

%token SYM INST

/* plain ID */
%token <string> PLAIN_ID

/* "dollar" (indexed) ID */
%token <string> IDX_ID

%token <int> NUMBER

/* in ascending order of priority */
%nonassoc BAR
%left IFF
%left OR
%right IMPLIES
%right ELSE
%left AND
%left RELEASE SINCE UNTIL 
%nonassoc NOT NEXT ALWAYS SOMETIME PREVIOUS HISTORICALLY ONCE
%nonassoc /*LT LTE GT GTE*/ EQ NEQ IN NOT_IN
//%nonassoc NO SOME LONE ONE      (* for formulas as 'some E' (= E != none) *)
%left MINUS PLUS
%nonassoc HASH
%left OVERRIDE
%left INTER
%right ARROW
%left LPROJ
%left RPROJ
%left LBRACKET                  (* for box join *)
%left DOT
%nonassoc TILDE HAT STAR
%nonassoc PRIME

%%

%public parse_problem:
  urelts_list = universe decls = possible_declarations
    insts? syms? 
    gs = goal+ EOF
	  { (urelts_list, decls, gs) }



  ////////////////////////////////////////////////////////////////////////
  // universe
  ////////////////////////////////////////////////////////////////////////

universe:
	UNIV COLON urelts_list = braces(urelements*)
	{ urelts_list }

urelements:
  i = interval
	{ R.uintvl i }
  | at = PLAIN_ID
  | at = IDX_ID
  { R.uplain @@ Raw_ident.ident at $startpos $endpos }

/* One may only declare univ. However, if other relations are also declared,
  then the univ declaration must first be followed by a SEMI */
possible_declarations:
  /* empty */
  { [] }
  |
  /* Besides, relation declarations must be separated by a SEMI, the last one being optional */
   SEMI decls = right_flexible_list(SEMI, declaration)
  { decls }
  
declaration:
	CONST? id = PLAIN_ID COLON sc = scope 
	{ R.dconst (Raw_ident.ident id $startpos(id) $endpos(id)) sc }
  |
	VAR id = PLAIN_ID COLON sc = scope fby = next_scope? 
	{ R.dvar (Raw_ident.ident id $startpos(id) $endpos(id)) sc fby }

next_scope: THEN sc = scope 
  { sc }

%inline scope: 
	b = bound
	{ R.sexact b }
	| b1 = bound b2 = bound
	{ R.sinexact b1 b2}

bound: 
	UNIV
	{ R.buniv }
  | id = PLAIN_ID     
  { R.bref (Raw_ident.ident id $startpos(id) $endpos(id)) }
	| b = parens(bound)
	{ b }
	| b1 = bound ARROW b2 = bound
	{ R.bprod b1 b2 }
	| b1 = bound PLUS b2 = bound
	{ R.bunion b1 b2  }
  | elts = braces(element*)
	{ R.belts elts }

element:
  i = interval  /* necessarily: at least two 1-tuples */
  { R.eintvl i }
  | t = tuple    /* one parenthesised tuple of any arity >= 1 is possible */
  { R.etuple t }
  | at = atom    /* a single atom without parentheses */
  { R.etuple [at] }
  

tuple:
  ats = parens(atom+)
  { ats }
  
interval:
  at1 = IDX_ID
  DOTDOT
  at2 = IDX_ID
  {
    R.interval (Raw_ident.ident at1 $startpos(at1) $endpos(at1))
      (Raw_ident.ident at2 $startpos(at2) $endpos(at2))
 } 

atom:
  at = PLAIN_ID
 | at = IDX_ID
 { Raw_ident.ident at $startpos $endpos } 




  ////////////////////////////////////////////////////////////////////////
  // instances
  ////////////////////////////////////////////////////////////////////////

insts:
 INST
 right_flexible_list(SEMI, inst)
 {}

inst:
 PLAIN_ID EQ braces(tuple*) 
 {}

 


  ////////////////////////////////////////////////////////////////////////
  // symmetries
  ////////////////////////////////////////////////////////////////////////

syms:
  SYM right_flexible_list(SEMI, sym)
  {}

sym:
  sym_element+ LTE sym_element+ 
  {}

sym_element:
  brackets(atom+)
  {}
    
  ////////////////////////////////////////////////////////////////////////
  // goal
  ////////////////////////////////////////////////////////////////////////
 
%inline goal:
	SAT fs = specification
	{ G.sat fs }

specification:
	fs = right_flexible_list(SEMI, formula)
	{ fs }

formula :
/*  exp = FBUILTIN args = brackets(comma_sep1(expr))
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.fbuiltin exp args} 
  
 |*/
     TRUE
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.true_ }
  
	| FALSE
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.false_ }
  
	| qual = rqualify e = expr
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.qual qual e }
  
	| e1 = expr op = comp_op e2 = expr
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.rcomp e1 op e2 }
  
	| e1 = iexpr op = icomp_op e2 = iexpr
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.icomp e1 op e2 }

	| op = lunop f = formula
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.lunary op f }
  
  | f1 = formula op = lbinop f2 = formula
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.lbinary f1 op f2 }
	
	| q = ae_quant decls = comma_sep1(ae_decl) block = f_block_or_bar
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.qaen q decls block }
  
	| q = lo_quant decls = comma_sep1(lo_decl) block = f_block_or_bar
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.qlo q decls block }
  
	| LET decls = comma_sep1(let_decl) block = f_block_or_bar
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.let_ decls block}
      
	| f = formula IMPLIES t = formula ELSE e = formula
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.fite f t e }
      
	| f = f_block
	{ G.fml (Location.from_positions $startpos $endpos)
    @@ G.block f }
  
	| f = parens(formula)
	    { f }

  
%inline ae_quant:
	ALL
	{ G.all }
	| SOME
	{ G.some }
	| NO
	{ G.no_ }

%inline lo_quant:
	ONE
	{ G.one }
  | LONE
	{ G.lone }

%inline ae_decl:
	disj = iboption(DISJ) ids = comma_sep1(plain_id) COLON range = expr
	{ (disj, ids, range) }

%inline plain_id:
  id = PLAIN_ID
 	{ Raw_ident.ident id $startpos(id) $endpos(id) }
  
%inline lo_decl:
 	id = PLAIN_ID COLON e = expr
 	{ (Raw_ident.ident id $startpos(id) $endpos(id), e) }

%inline f_block_or_bar:
 	BAR f = formula
	{ [f] }
	| block = f_block
	{ block }

%inline f_block:
	 fs = braces(right_flexible_list(SEMI, formula))
	{  fs }

%inline lbinop:
	AND
	{ G.and_ }
	| OR
	{ G.or_ }
	| IMPLIES
	{ G.impl }
	| IFF
	{ G.iff }
	| UNTIL
	{ G.until }
	| RELEASE
	{ G.release }
	| SINCE
	{ G.since }

%inline lunop:
	SOMETIME
	{ G.sometime }
	| ALWAYS
	{ G.always }
	| NOT
	{ G.not_ }
	| ONCE
	{ G.once }
	| NEXT
	{ G.next }
	| PREVIOUS
	{ G.previous }
	| HISTORICALLY
	{ G.historically }
    

    ////////////////////////////////////////////////////////////////////////
    // RELATIONAL EXPRESSIONS
    ////////////////////////////////////////////////////////////////////////
  
expr:
  NONE 
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.none }
  
	| UNIV
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.univ}
  
	| IDEN
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.iden }
  
/*	| INT
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.int }*/
  
  | id = PLAIN_ID
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.ident @@ Raw_ident.ident id $startpos $endpos}
      
	| op = runop e = expr
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.runary op e }
  
	| e1 = expr op  = rbinop e2 = expr
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.rbinary e1 op e2 }
  
	| f = formula IMPLIES t = expr ELSE e = expr
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.rite f t e}
  
	| exp = expr args = brackets(comma_sep1(expr))
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.boxjoin exp args }
  
	| compr = braces(compr_body)
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ compr}
  
	| e = expr PRIME
	{ G.exp (Location.from_positions $startpos $endpos)
    @@ G.prime e}
  
	| e = parens(expr)
	    { e }

      

%inline comp_op:
 	NOT_IN
	{ G.not_in}
  | IN
	{ G.in_ }
  | EQ
	{ G.req } 
  | NEQ
 	{ G.rneq }

%inline rqualify:
 	ONE
	{ G.rone }
  | LONE
	    { G.rlone }
  | SOME
	    { G.rsome }
  | NO
 	{ G.rno }

%inline runop:
	TILDE
	{ G.transpose }
  | HAT
	    { G.tclos }
  | STAR
	{ G.rtclos }

%inline rbinop:
	PLUS
	{ G.union }
	| INTER
	{ G.inter }
	| OVERRIDE
	{ G.over }
	| LPROJ
	{ G.lproj }
	| RPROJ
	{ G.rproj }
	| ARROW
	{ G.prod }
	| MINUS
	{ G.diff }
	| DOT
	{ G.join }

%inline compr_body:
	decls = comma_sep1(ae_decl) block = f_block_or_bar
	    { G.compr decls block }

%inline let_decl:
	id = PLAIN_ID EQ e = expr
	{ (Raw_ident.ident id $startpos(id) $endpos(id), e) }
 
 
    ////////////////////////////////////////////////////////////////////////
    // INTEGER EXPRESSIONS
     ////////////////////////////////////////////////////////////////////////
  
iexpr:
  n = NUMBER
  { G.exp (Location.from_positions $startpos $endpos)
    @@ G.num n  }
  | HASH e = expr
  { G.exp (Location.from_positions $startpos $endpos)
    @@ G.card e  }
  | NEG e = brackets(iexpr)
  { G.exp (Location.from_positions $startpos $endpos)
    @@ G.(iunary neg e) } 
  | ADD e = brackets(two_iexprs)
      {
        let (e1, e2) = e in
        G.exp (Location.from_positions $startpos $endpos)
    @@ G.(ibinary e1 add e2)  }
  | SUB e = brackets(two_iexprs)
  { 
    let (e1, e2) = e in
    G.exp (Location.from_positions $startpos $endpos)
    @@ G.(ibinary e1 sub e2)  } 

%inline two_iexprs:
  e1 = iexpr COMMA e2 = iexpr
  { (e1, e2) }
  
  icomp_op:
  | LT
	{ G.lt}
	| LTE
	{ G.lte }
	| GT
	{ G.gt } 
	| GTE
	{ G.gte }
  | EQ
	{ G.ieq } // TODO 
  | NEQ
 	{ G.ineq } // TODO


    ////////////////////////////////////////////////////////////////////////
    // MENHIR MACROS
    ////////////////////////////////////////////////////////////////////////
  
(* Given by François Pottier on 2015-01-21
   at http://gallium.inria.fr/blog/lr-lists/ *)
%public right_flexible_list(delim, X):
	| (* nothing *)
	    { [] }
	| x = X
	    { [x] }
	| x = X delim xs = right_flexible_list(delim, X)
	    { x :: xs }

%public %inline comma_sep1(X) :
  xs = separated_nonempty_list(COMMA, X)
    { xs }


    
%public %inline braces(X):
  x = delimited(LBRACE, X, RBRACE)
    { x }

    
%public %inline brackets(X):
  x = delimited(LBRACKET, X, RBRACKET)
    { x }

%public %inline parens(X):
  x = delimited(LPAREN, X, RPAREN)
    { x }


%public %inline iboption(X):
 (* empty *)
 { false }
 | X
 { true }


    
