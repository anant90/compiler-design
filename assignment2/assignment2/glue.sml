 structure LogicLrVals : Logic_LRVALS= 
LogicLrValsFun(
 	structure Token = LrParser.Token
 	structure Abs = Abs);

 structure LogicLex = 
	LogicLexFun(structure Tokens = LogicLrVals.Tokens);

 structure LogicParser = JoinWithArg(
 	structure Abs = Abs
	structure ParserData = LogicLrVals.ParserData
 	structure Lex=LogicLex
 	structure LrParser=LrParser);
