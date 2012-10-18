open Abs
%% 

%header (functor LogicLrValsFun (structure Token : TOKEN
                               structure Abs : ABS ):Logic_LRVALS)

%term EMPTY 
	| COMMA
	| IMPLIES
	| PERIOD 
	| NAME of string 
	| VARIABLE of string 
	| INTEGER of string
	| REAL of string
	| NEG
	| TRUE
	| FALSE
	| PLUS
	| MINUS
	| MULT
	| DIV
	| GT
	| LT
	| GTE
	| LTE
	| NOT
	| AND
	| OR
	| EQUALS
	| LPAREN
	| RPAREN
	| LBRACKET
	| RBRACKET
	| EOF
	| PROJ
	| FUN
	| FUNAPPLY
	| IF
	| THEN
	| ELSE
	| FI
	| LET
	| IN
	| END
%nonterm EXPR of Expr
	| START of Expr
	| BEXPR of Expr
	| BTERM of Expr
	| BBTERM of Expr
	| BFACTOR of Expr
	| AEXPR of Expr
	| TERM of Expr
	| FACTOR of Expr
	| FACTOR1 of Expr
	| FACTOR2 of Expr
	| TUPLE of Tuple

%eop EOF
%noshift EOF
%pos int
%verbose
%pure
%arg (fileName) : string
%name Logic
%%

START: EXPR (EXPR)

EXPR: BEXPR			(expr(BEXPR))
	| AEXPR			(expr(AEXPR))
	| LBRACKET TUPLE RBRACKET 	(tuple(TUPLE))
	| PROJ TUPLE INTEGER	(proj(TUPLE,INTEGER))
	| FUN EXPR IMPLIES EXPR  (function(EXPR,EXPR))
	| FUNAPPLY EXPR PERIOD EXPR (funApply(EXPR,EXPR))
	| IF EXPR THEN EXPR ELSE EXPR FI (ifthenelse(EXPR,EXPR,EXPR))
	| LET EXPR EQUALS EXPR IN EXPR END (letinend(EXPR,EXPR,EXPR))
	|			(empty)

BEXPR: BTERM			(expr(BTERM))
	| BTERM OR BEXPR 	(binOr(BTERM,BEXPR))

BTERM: BBTERM			(expr(BBTERM))
	| BBTERM AND BTERM	(binAnd(BBTERM,BTERM))

BBTERM: BFACTOR			(expr(BFACTOR))
	| NOT LPAREN BEXPR RPAREN (not(BEXPR))

TUPLE: EXPR COMMA TUPLE        (etuple(EXPR,TUPLE))
	| AEXPR			(e(AEXPR))

BFACTOR: TRUE			(true)
	| FALSE			(false)
	| VARIABLE		(variable(VARIABLE))
	| AEXPR GT AEXPR	(gt(AEXPR,AEXPR))
	| AEXPR GTE AEXPR       (gte(AEXPR,AEXPR))
	| AEXPR LT AEXPR        (lt(AEXPR,AEXPR))
	| AEXPR LTE AEXPR       (lte(AEXPR,AEXPR))
	| AEXPR EQUALS AEXPR	(equals(AEXPR,AEXPR))

AEXPR: TERM 			(expr(TERM))
	| TERM PLUS AEXPR 	(add(TERM,AEXPR))
	| TERM MINUS AEXPR 	(sub(TERM,AEXPR))
	| 			(empty)

TERM: FACTOR			(expr(FACTOR))
	| FACTOR MULT TERM 	(mul(FACTOR,TERM))
	| FACTOR DIV TERM 	(divide(FACTOR,TERM))

FACTOR: FACTOR1			(expr(FACTOR1))
	| NEG FACTOR1		(neg(FACTOR1))

FACTOR1: FACTOR2		(expr(FACTOR2))
	| LPAREN EXPR RPAREN	(expr(EXPR))

FACTOR2: INTEGER 		(n(INTEGER))
	| REAL			(r(REAL))
	| VARIABLE		(variable(VARIABLE))
