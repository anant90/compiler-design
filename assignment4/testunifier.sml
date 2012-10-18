use "unifier.sml";
Control.Print.printDepth := 20;
val (sig1:sign) = [("not", 1), ("and", 2), ("or",2), ("true", 0), ("false", 0)];

val t1 = [Name "and", Arguments [[Name "or", Arguments [[Name "Unknown"], [Name "true"]]]]];

isValidSign sig1; (* true *)
isValidSign [("Test",1)];  (* false since capital letter starts the non-variable name *)

arity sig1 "and";
arity sig1 "or";
arity sig1 "true";
arity sig1 "Unknown";

isValidTerm sig1 [Name "true", Arguments [[Name "true"]]]; (* false *)
isValidTerm sig1 [Name "and", Arguments [[Name "true"], [Name "Unknown"]]]; (* true *)

val t1 = [Name "and", Arguments [[Name "or", Arguments [[Name "Unknown"], [Name "false"]]]]];
isValidTerm sig1 t1;

val t2 = [Name "not", Arguments [[Name "not", Arguments [[Name "false"]] ]] ];
isValidTerm sig1 t2;

(* raises notAValidArg exception*)

(subst t1 [Name "Unknown"] t1);

val sol1 = findmgu [Name "not", Arguments [[Name "Un"]]]
                  [Name "not", Arguments [[Name "and", Arguments [[Name "Var1"], [Name "Var2"]]]]];

val sol2 = findmgu [Name "a", Arguments [ [Name "or", Arguments [[Name "X"],[Name "true"]]],[Name "Y"]]] 
		[Name "a", Arguments [[Name "Z"], [Name "or", Arguments [[Name "false"], [Name "X"]]]]]; 

(*Unification must fail since X belongs to vars(or) *)
val sol3 = findmgu [Name "and", Arguments [ [Name "or", Arguments [[Name "true"],[Name "X"]]],[Name "X"]]]
                [Name "and", Arguments [[Name "Y"], [Name "or", Arguments [[Name "Y"], [Name "false"]]]]];
