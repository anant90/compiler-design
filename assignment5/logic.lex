structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
type lexarg = string
type arg = lexarg

val lineno = ref 1;
val colno = ref 1;
val eof = fn (fileName:string)=> Tokens.EOF(!lineno,!colno)
val error = fn (e,l : int,_) =>
              print("line " ^ (Int.toString(l)) ^
                               ": " ^ e ^ "\n")
%%
%full
%header (functor LogicLexFun(structure Tokens: Logic_TOKENS));
%arg (fileName:string);
alphanum = [A-Za-z0-9_'];
capitals = [A-Z];
smalls = [a-z];
ws      = [\ \t];
digit = [0-9]; 
nonZeroDigit = [1-9];


%%
\n       => (lineno := (!lineno) + 1; colno:=1; continue());
{capitals}({alphanum}*[A-Za-z0-9'])*	=> (colno := (!colno) + String.size(yytext); Tokens.VARIABLE((yytext),!lineno,!colno));
{nonZeroDigit}({digit})*	=> (colno := (!colno) + String.size(yytext); Tokens.INTEGER((yytext),!lineno,!colno)); 
(0 | ({nonZeroDigit}({digit})*))"."((0)*{nonZeroDigit})+ => (colno := (!colno) + String.size(yytext); Tokens.REAL((yytext),!lineno,!colno));
"true" 	=> (colno:=(!colno) + 1; Tokens.TRUE(!lineno,!colno));
"false" => (colno:=(!colno) + 1; Tokens.FALSE(!lineno,!colno));
"proj"	=> (colno:=(!colno) + 1; Tokens.PROJ(!lineno,!colno));
"function" => (colno:=(!colno) + 1; Tokens.FUN(!lineno,!colno));
"funApply" => (colno:=(!colno) + 1; Tokens.FUNAPPLY(!lineno,!colno));
"if"	=> (colno:=(!colno) + 1; Tokens.IF(!lineno,!colno)); 
"then"	=> (colno:=(!colno) + 1; Tokens.THEN(!lineno,!colno));
"else"	=> (colno:=(!colno) + 1; Tokens.ELSE(!lineno,!colno));
"fi"	=> (colno:=(!colno) + 1; Tokens.FI(!lineno,!colno));
"let" 	=> (colno:=(!colno) + 1; Tokens.LET(!lineno,!colno));
"in"	=> (colno:=(!colno) + 1; Tokens.IN(!lineno,!colno));
"end"	=> (colno:=(!colno) + 1; Tokens.END(!lineno,!colno));
"+"	=> (colno:=(!colno) + 1; Tokens.PLUS(!lineno,!colno));
"-"	=> (colno:=(!colno) + 1; Tokens.MINUS(!lineno,!colno));
"~"     => (colno:=(!colno) + 1; Tokens.NEG(!lineno,!colno));
"*"	=> (colno:=(!colno) + 1; Tokens.MULT(!lineno,!colno));
"div"	=> (colno:=(!colno) + 1; Tokens.DIV(!lineno,!colno));
"&&"	=> (colno:=(!colno) + 1; Tokens.AND(!lineno,!colno));
"||"	=> (colno:=(!colno) + 1; Tokens.OR(!lineno,!colno));
">"	=> (colno:=(!colno) + 1; Tokens.GT(!lineno,!colno));
"<"	=> (colno:=(!colno) + 1; Tokens.LT(!lineno,!colno));
">="	=> (colno:=(!colno) + 1; Tokens.GTE(!lineno,!colno));
"<="	=> (colno:=(!colno) + 1; Tokens.LTE(!lineno,!colno));
"not"	=> (colno:=(!colno) + 1; Tokens.NOT(!lineno,!colno));
"="	=> (colno:=(!colno) + 1; Tokens.EQUALS(!lineno,!colno));
"("      => (colno:=(!colno) + 1; Tokens.LPAREN(!lineno,!colno));
")"      => (colno:=(!colno) + 1; Tokens.RPAREN(!lineno,!colno));
"["      => (colno:=(!colno) + 1; Tokens.LBRACKET(!lineno,!colno));
"]"      => (colno:=(!colno) + 1; Tokens.RBRACKET(!lineno,!colno));
"=>"	=> (colno:=(!colno) + 1; Tokens.IMPLIES(!lineno,!colno));
","      => (colno:=(!colno) + 1; Tokens.COMMA(!lineno,!colno));
"."      => (colno:=(!colno) + 1; Tokens.PERIOD(!lineno,!colno));
{ws}+	 => (colno := (!colno) + 1; continue());
.	 => (colno:=(!colno) + 1;error ("ignoring bad character "^yytext,!lineno,!colno);continue());        

