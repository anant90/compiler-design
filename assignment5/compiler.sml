(* compiler.sml *)
 structure Logic :
  sig val compile : string -> Abs.Expr
 end =
 struct
 exception LogicError;
 fun compile (fileName) =
 	let val inStream = TextIO.openIn fileName;
 		val grab : int -> string = fn
 			n => if TextIO.endOfStream inStream
				then ""
 				else TextIO.inputN (inStream,n);
 		val printError : string * int * int -> unit = fn
			(msg,line,col) =>
 			print (fileName^"["^Int.toString line^":"
 				^Int.toString col^"] "^msg^"\n");
 		val (tree,rem) = LogicParser.parse
 			(15,
 			(LogicParser.makeLexer grab fileName),
 			printError,
 			fileName)
 		handle LogicParser.ParseError => raise LogicError;
 		(* Close the source program file *)
 		val _ = TextIO.closeIn inStream;
 	in tree
 	end
 end;
