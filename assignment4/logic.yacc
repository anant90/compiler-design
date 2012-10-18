open Abs
%% 

%header (functor LogicLrValsFun (structure Token : TOKEN
                               structure Abs : ABS ):Logic_LRVALS)

%term EMPTY 
	| COMMA
	| PERIOD 
	| NAME of string 
	| VARIABLE of string 
	| LPAREN
	| RPAREN
	| IF
	| EOF
%nonterm PROGRAM of Program
	| START of Program
	| STATEMENT of Statement
	| FACT of Fact
	| RULE of Rule
	| HEAD of Head
	| BODY of Body
	| LITERAL of Literal 
	| TUPLE of Tuple
	| TERM of Term 
	| TERMS of Terms

%eop EOF
%noshift EOF
%pos int
%verbose
%pure
%arg (fileName) : string
%name Logic
%%

START: PROGRAM (PROGRAM)

PROGRAM: STATEMENT PROGRAM 	(program(STATEMENT,PROGRAM))
	|			(empty)

STATEMENT: FACT PERIOD (fact(FACT))
	| RULE PERIOD (rule(RULE))

FACT: LITERAL (literal(LITERAL))
RULE: HEAD IF BODY (headbody(HEAD,BODY))
HEAD: LITERAL (head(LITERAL))
BODY: LITERAL (body(LITERAL))
	| LITERAL COMMA BODY (literalbody(LITERAL,BODY))
LITERAL: NAME (lname(NAME))
	| NAME TUPLE (nametuple(NAME,TUPLE))
TUPLE: LPAREN TERMS RPAREN (tuple(TERMS))
TERMS: TERM (term(TERM))
	| TERM COMMA TERMS (terms(TERM,TERMS))
TERM: NAME (tname(NAME))
	| VARIABLE (tvar(VARIABLE)) 
	| NAME TUPLE (ttot(NAME,TUPLE))

