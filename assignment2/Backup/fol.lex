structure Tokens = Tokens
structure Interface = Interface
open Interface

type pos = Interface.pos
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val eof = fn () => Tokens.EOF(!line,!line)
fun makeInt (s : string) = s

%%
%header (functor FolLexFun(structure Tokens: Fol_TOKENS
			   structure Interface: INTERFACE) : LEXER);
alphanum=[A-Za-z0-9_'];
ws=[\ \t\n];

%%
<INITIAL>{ws}	=> (lex());
<INITIAL>\n	=> (next_line(); lex());
<INITIAL>[A-Z]({alphanum}*[A-Za-z0-9'])*	=> (Tokens.VARIABLE(yytext,!line,!line));
<INITIAL>[a-z]({alphanum}*[A-Za-z0-9'])*	=> (Tokens.NAME(yytext,!line,!line));
<INITIAL>":-"	=> (Tokens.IF(!line,!line));
<INITIAL>","	=> (Tokens.COMMA(!line,!line));
<INITIAL>"."    => (Tokens.DOT(!line,!line));
<INITIAL>"("	=> (Tokens.LPAREN(!line,!line));
<INITIAL>")"	=> (Tokens.RPAREN(!line,!line));
<INITIAL>.	=> (error ("ignoring illegal character" ^ yytext,
			   !line,!line); lex());
