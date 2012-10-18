Control.Print.printDepth := 20;
use "secd.sml";

(***** Q1 *****)
val a = Num 2;
val e1 = Apply(Fn("L1",AtomicPat "x",Fn("L2",AtomicPat "f",Apply(Var "f", Var "x"))),Apply(Fn("L3",AtomicPat "x",Var "x"),a));
val cbv1 = eval(e1);

(***** Q2 *****)

(* use AtomicPat
val Ycbn = Apply(Fn("L1","f",Fn("L2","x",Apply(Var "f", Apply(Var "x",Var "x")))),Fn("L3","x",Apply(Var "f", Apply(Var "x",Var "x"))) );
val Ycbv = Apply(Fn("L1","f",Fn("L2","x",Apply(Apply(Apply(Fn("L3","y",Var "x"),Var "x"),Var "x"),Var "y"))),Fn("L4","x",Apply(Apply(Apply(Fn("L5","y",Var "x"),Var "x"),Var "x"),Var "y")));
*)

val factfn =  Fn("f",AtomicPat "x",
               If(Prim(EQUAL,Var "x", Num 0),
                  Num 1,
                  Prim(TIMES,Var "x",
                       Apply(Var "f", Prim(MINUS, Var "x", Num 1)))));

val fact5cbv = eval(Apply(factfn, Num 5))

(***** Q3 *****)
val a = [Num 3,Bool true];
val b = [Num 5, Num 9]; val c = a;
val expr3 = Fn("g", AtomicPat "x",Proj(c,Var "x"));
val expr31 = Apply(expr3, Num 1);
val expr32 = Apply(expr3, Num 2);
val output31 = eval(expr31);
val output32 = eval(expr32);

(***** Q4 *****)

val expr4 = Fn("L1", TuplePat ["x", "y"], If(Prim(AND, Var "x", Var "y"), expr31, expr32));
val output4 = eval(Apply(expr4,Tuple [Bool true, Bool true]));

