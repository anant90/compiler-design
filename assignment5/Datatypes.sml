signature ABS =
sig datatype Expr = empty
		| expr of Expr
                | variable of string
		| neg of Expr
                | add of Expr*Expr
                | sub of Expr*Expr
                | mul of Expr*Expr
                | divide of Expr*Expr
                | gt of Expr*Expr
                | lt of Expr*Expr
                | gte of Expr*Expr
                | lte of Expr*Expr
		| not of Expr
		| equals of Expr*Expr
                | binAnd of Expr*Expr
                | binOr of Expr*Expr
                | n of string
                | r of string
                | true
                | false
                | tuple of Tuple
                | proj of Tuple*string
                | ifthenelse of Expr*Expr*Expr
                | function of Expr*Expr
		| funApply of Expr*Expr
		| letinend of Expr*Expr*Expr
        and Tuple = e of Expr
                | etuple of Expr*Tuple
end;

structure Abs :> ABS =
struct datatype Expr = empty
		| expr of Expr
                | variable of string
		| neg of Expr
                | add of Expr*Expr
                | sub of Expr*Expr
                | mul of Expr*Expr
                | divide of Expr*Expr
                | gt of Expr*Expr
                | lt of Expr*Expr
                | gte of Expr*Expr
                | lte of Expr*Expr
		| not of Expr
		| equals of Expr*Expr
                | binAnd of Expr*Expr
                | binOr of Expr*Expr
                | n of string
                | r of string
                | true
                | false
                | tuple of Tuple
                | proj of Tuple*string
                | ifthenelse of Expr*Expr*Expr
                | function of Expr*Expr
		| funApply of Expr*Expr
		| letinend of Expr*Expr*Expr
	and Tuple = e of Expr
		| etuple of Expr*Tuple
end;

