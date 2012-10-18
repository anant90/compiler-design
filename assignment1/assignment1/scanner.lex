datatype lexresult = VARIABLE of string
		| NAME of string
		| INTEGER of string
		| CONSTANT of string
		| LPAREN | RPAREN
		| COMMA 
		| PERIOD
		| WS
		| OP of string
		| COMPARATOR of string
		| ASSIGN
		| IF
		| LBRACKET
		| RBRACKET
		| BAR
		| EOF

val linenum = ref 1
val error = fn x => output(stdOut,x ^ "\n")
val eof = fn () => EOF

%%
%structure Scanner
alpha = [A-Za-z];
capitals = [A-Z];
smalls = [a-z];
digit = [0-9]; 
nonZeroDigit = [1-9];
ws      = [\ \t\n];
operator = [-+/*]|(mod)|(div);
comparator = ([\>\<])?(\=)?;

%%
{capitals}((_)*({alpha}|')+)*	=> (VARIABLE yytext);
{smalls}((_)*({alpha}|')+)*	=> (NAME yytext);
{nonZeroDigit}({digit})*	=> (INTEGER yytext); 
("0" | {nonZeroDigit}({digit})*).((0)*{nonZeroDigit})+ => (CONSTANT yytext);
"("		=> (LPAREN);
")"		=> (RPAREN);
","		=> (COMMA);
"."		=> (PERIOD);
{ws}+		=> (WS);
{operator}	=> (OP yytext);
{comparator}	=> (COMPARATOR yytext);
":-"		=> (ASSIGN);
"if"		=> (IF);
"["		=> (LBRACKET);
"]"		=> (RBRACKET);
"|"		=> (BAR);
.               => (error ("scanner: ignoring bad character "^yytext); lex());
