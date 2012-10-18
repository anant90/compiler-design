signature ABSYN =
sig datatype	Program = PR of Statement*Program
			 |EMPTY
and		Statement = FT of Fact
			   | RL of Rule
and		Fact = LT of Literal
and		Rule = HB of Head*Body
and		Head = HD of Literal
and		Body = BD of Literal
			| LB of Literal*Body
and 		Literal = LNM of string
			| LNT of string*Tuple
and 		Tuple =	TU of Lterms
and		Lterms = L1 of Terms
			| L2 of Terms*Lterms
and 		Terms = T1 of string
			| T2 of string
			| T3 of string*Tuple
end;
structure Absyn :> ABSYN =
struct
  datatype Program = PR of Statement*Program
			 |EMPTY
and		Statement = FT of Fact
			   | RL of Rule
and		Fact = LT of Literal
and		Rule = HB of Head*Body
and		Head = HD of Literal
and		Body = BD of Literal
			| LB of Literal*Body
and 		Literal = LNM of string
			| LNT of string*Tuple
and 		Tuple =	TU of Lterms
and		Lterms = L1 of Terms
			| L2 of Terms*Lterms
and 		Terms = T1 of string
			| T2 of string
			| T3 of string*Tuple
end;











