(*Demo File*)
(*Anant Jain, 2008EE10330, Group 2 *)

use "main.sml";

(* Part a: *)

val (tA:typeAssumption) = [(Abs.variable("X"),[Int]),(Abs.variable("Y"),[Int]) ];
val (tbl:table) = [(Abs.variable("X"),[integer(6)]),(Abs.variable("Y"),[integer(3)])];
val a = Logic.compile "exprA";
val typeA = hasType (a,tA);
val valueA = eval (a,tbl);

val b = Logic.compile "exprB";
val typeB = hasType (b,tA);
val valueB = eval (b,tbl);
val (tA:typeAssumption) = [(Abs.variable("X"),[Bool]),(Abs.variable("Y"),[Bool]) ];
val (tbl:table) = [(Abs.variable("X"),[boolean(true)]),(Abs.variable("Y"),[boolean(false)])];

val c1 = Logic.compile "exprCa";
val c2 = Logic.compile "exprCb";
val c = Abs.binAnd(c1,c2);
val typeC = hasType (c,tA);
val valueC = eval (c,tbl);

val (tA:typeAssumption) = [(Abs.variable("X"),[Int]),(Abs.variable("Y"),[Bool]) ];
val (tbl:table) = [(Abs.variable("X"),[integer(6)]),(Abs.variable("Y"),[boolean(false)])];
val d = Abs.function(Abs.variable("X"),Abs.function(Abs.variable("Y"),Abs.tuple (Abs.etuple(Abs.tuple (Abs.etuple(Abs.variable "X", Abs.e(Abs.r "5.0"))),Abs.etuple(Abs.variable "Y", Abs.e(Abs.n "4")) ) )))
val typeD = hasType (d,tA);
val evalD = Abs.funApply (Abs.function(Abs.variable("X"),Abs.funApply (Abs.function(Abs.variable("Y"),Abs.tuple (Abs.etuple(Abs.tuple (Abs.etuple(Abs.variable "X", Abs.e(Abs.r "5.0"))),Abs.etuple(Abs.variable "Y", Abs.e(Abs.n "4"))))),Abs.false )),Abs.n ("6"))
val valueD = eval (evalD, tbl);

val (tA:typeAssumption) = [(Abs.variable("X"),[Int]),(Abs.variable("Y"),[Int]) ];
val (tbl:table) = [(Abs.variable("X"),[integer(3)]),(Abs.variable("Y"),[integer(7)])];
val e = Abs.letinend(Abs.variable("X"), Abs.n("3"), Abs.letinend(Abs.variable ("Y"),Abs.n "7", Abs.add(Abs.variable "X", Abs.variable "Y")));
val typeE = hasType (e,tA);
val valueE = eval (e,tbl);




