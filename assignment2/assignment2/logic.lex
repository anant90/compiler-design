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

%%
\n       => (lineno := (!lineno) + 1; colno:=1; continue());
{capitals}({alphanum}*[A-Za-z0-9'])*	=> (colno := (!colno) + String.size(yytext); Tokens.VARIABLE((yytext),!lineno,!colno));
{smalls}({alphanum}*[A-Za-z0-9'])*	=> (colno := (!colno) + String.size(yytext); Tokens.NAME((yytext),!lineno,!colno));
"("      => (colno:=(!colno) + 1; Tokens.LPAREN(!lineno,!colno));
")"      => (colno:=(!colno) + 1; Tokens.RPAREN(!lineno,!colno));
","      => (colno:=(!colno) + 1; Tokens.COMMA(!lineno,!colno));
"."      => (colno:=(!colno) + 1; Tokens.PERIOD(!lineno,!colno));
{ws}+	 => (colno := (!colno) + 1; continue());
":-"	 => (colno:=(!colno) + 1; Tokens.IF(!lineno,!colno));
.	 => (colno:=(!colno) + 1;error ("ignoring bad character "^yytext,!lineno,!colno);continue());        

