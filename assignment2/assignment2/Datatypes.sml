signature ABS =
sig datatype Program = program of Statement*Program
			| empty
	and Statement = fact of Fact
			| rule of Rule
	and Fact = literal of Literal
	and Rule = headbody of Head*Body
	and Head = head of Literal
	and Body = body of Literal
			| literalbody of Literal*Body
	and Literal = lname of string
			| nametuple of string*Tuple
	and Tuple = tuple of Terms
	and Terms = term of Term
			| terms of Term*Terms
	and Term  = tname of string
			| tvar of string
			| ttot of string*Tuple
end;

structure Abs :> ABS = 
struct datatype Program = program of Statement*Program
                        | empty
        and Statement = fact of Fact
                        | rule of Rule
        and Fact = literal of Literal
        and Rule = headbody of Head*Body
        and Head = head of Literal
        and Body = body of Literal
                        | literalbody of Literal*Body
        and Literal = lname of string
                        | nametuple of string*Tuple
        and Tuple = tuple of Terms
        and Terms = term of Term
                        | terms of Term*Terms
        and Term  = tname of string
                        | tvar of string
                        | ttot of string*Tuple
end;
