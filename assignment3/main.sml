Control.Print.printDepth := 20;
use "unifier.sml";
val (sig1:sign) = [("zero", 0), ("s", 1), ("add",2)];

val t1 = [ Name "add", Arguments [ [Name "s", Arguments [[Name "s", Arguments [[Name "zero"]]]]], [Name "s", Arguments [[Name "X"]]]]];

val t2 = [Name "add", Arguments [ [Name "X"], [Name "add", Arguments [[Name "X"],[Name "Y"] ]]]];

val t3 = [Name "s", Arguments [[Name "s", Arguments [[Name "zero"]]],[Name "s", Arguments [[Name "zero"]]] ]];

isValidTerm sig1 t1;
isValidTerm sig1 t2;
isValidTerm sig1 t3;

val w1 = [([Name "X"],[Name "zero"])];
val w2 = [([Name "Y"],[Name "s", Arguments [[Name "zero"]]])];
val c1 = compose (w1,w2);

val w3 = [([Name "X"],[Name "s", Arguments [[Name "zero"]]])];
val w4 = [([Name "X"],[Name "zero"])];
val c2 = compose (w3,w4);

val w5 = [([Name "X"],[Name "add", Arguments [[Name "X"],[Name "zero"]]])];
val w6 = [([Name "X"],[Name "s", Arguments [[Name "X"]]])];
val c3 = compose (w5,w6);

val w7 = [([Name "X"],[Name "s", Arguments [[Name "Y"]]]), ([Name "Z"],[Name "add", Arguments [[Name "X"],[Name "C"]]])];
val w8 = [([Name "Y"],[Name "Z"])];
val c4 = compose (w7,w8);

val t4 = [Name "add", Arguments [[Name "X"], [Name "add", Arguments [[Name "Y"], [Name "s", Arguments [[Name "X"]]]]]]];

val m1 = [Name "f", Arguments [[Name "a"],[Name "X"]]];
val m2 = [Name "f", Arguments [[Name "Y"],[Name "b"]]];
val sol1 = findmgu m1 m2;

val m1 = [Name "p", Arguments [[Name "f", Arguments [[Name "g", Arguments [[Name "X"],[Name "a"]]]]], [Name "X"]]];
val m2 = [Name "p", Arguments [[Name "Z"],[Name "b"]]];
val sol2 = findmgu m1 m2;

val m1 = [Name "p", Arguments [[Name "f", Arguments [[Name "X"], [Name "X"]]],[Name "Y"]]];
val m2 = [Name "p", Arguments [[Name "f", Arguments [[Name "a"], [Name "Z"]]],[Name "b"]]];
val sol3 = findmgu m1 m2;

val m1 = [Name "p", Arguments [[Name "X"], [Name "X"]]];
val m2 = [Name "p", Arguments [[Name "a"], [Name "b"]]];
(*
val sol4 = findmgu m1 m2;
*)
val m1 = [Name "p", Arguments [[Name "f", Arguments [[Name "X"], [Name "Y"]]],[Name "Z"]]];
val m2 = [Name "p", Arguments [[Name "Z"],[Name "f", Arguments [[Name "a"], [Name "Y"]]]]];
val sol5 = findmgu m1 m2;
