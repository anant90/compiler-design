Control.Print.printDepth := 20;
use "conversionFunctions.sml";

(*Define datatype for types *)
datatype typeConstituent =
	Bool
	| Int
	| Real
	| Unit

type Type = typeConstituent list

type typeAssumption = (Abs.Expr*Type) list;


datatype ansConstituent = 
	boolean of bool
	| integer of int
	| realNum of real
	| unitType;

type Answer = ansConstituent list

type table = (Abs.Expr*Answer) list;

exception typeException;

fun lookup (e,((v,t)::typeAssump):typeAssumption) =
	if (v=e) then t else lookup (e,typeAssump)
	| lookup (e,[]) = raise typeException;

exception valAssignFailed;

fun valAssign (e,((v,t)::tb):table) =
        if (v=e) then t else valAssign (e,tb)
        | valAssign (e,[]) = raise valAssignFailed;

fun hasType (Abs.variable(v), ta:typeAssumption) = lookup(Abs.variable(v),ta)
	| hasType(Abs.tuple(t), ta:typeAssumption) =  hasTypeTuple(t,ta)
	| hasType(Abs.empty, ta:typeAssumption) =  []
	| hasType(Abs.true, ta:typeAssumption) = [Bool]
	| hasType(Abs.false, ta:typeAssumption) = [Bool]
 	| hasType(Abs.n(e), ta:typeAssumption) = [Int]
	| hasType(Abs.r(e), ta:typeAssumption) = [Real]
	| hasType(Abs.add(e1,e2), ta:typeAssumption) = 
		if (hasType(e1,ta)=[Int] andalso hasType(e2,ta)=[Int]) 
		then [Int] 
		else 
			if (hasType(e1,ta)=[Real] andalso hasType(e2,ta)=[Real]) 
			then [Real] 
			else 
				raise typeException
	| hasType(Abs.neg(e1),ta:typeAssumption) = hasType(e1,ta)
	| hasType(Abs.sub(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Int] andalso hasType(e2,ta)=[Int])
                then [Int]
                else
                        if (hasType(e1,ta)=[Real] andalso hasType(e2,ta)=[Real])
                        then [Real]
                        else
                                raise typeException
	| hasType(Abs.mul(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Int] andalso hasType(e2,ta)=[Int])
                then [Int]
                else
                        if (hasType(e1,ta)=[Real] andalso hasType(e2,ta)=[Real])
                        then [Real]
                        else
                                raise typeException
	| hasType(Abs.divide(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Int] andalso hasType(e2,ta)=[Int])
                then [Int]
                else
                        if (hasType(e1,ta)=[Real] andalso hasType(e2,ta)=[Real])
                        then [Real]
                        else
                                raise typeException
	| hasType(Abs.gt(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Int] andalso hasType(e2,ta)=[Int])
                then [Bool]
                else
                        if (hasType(e1,ta)=[Real] andalso hasType(e2,ta)=[Real])
                        then [Bool]
                        else
                                raise typeException
	| hasType(Abs.gte(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Int] andalso hasType(e2,ta)=[Int])
                then [Bool]
                else
                        if (hasType(e1,ta)=[Real] andalso hasType(e2,ta)=[Real])
                        then [Bool]
                        else
                                raise typeException
	| hasType(Abs.lt(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Int] andalso hasType(e2,ta)=[Int])
                then [Bool]
                else
                        if (hasType(e1,ta)=[Real] andalso hasType(e2,ta)=[Real])
                        then [Bool]
                        else
                                raise typeException
	| hasType(Abs.lte(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Int] andalso hasType(e2,ta)=[Int])
                then [Bool]
                else
                        if (hasType(e1,ta)=[Real] andalso hasType(e2,ta)=[Real])
                        then [Bool]
                        else
                                raise typeException
	| hasType(Abs.equals(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Int] andalso hasType(e2,ta)=[Int])
                then [Bool]
                else
                        if (hasType(e1,ta)=[Real] andalso hasType(e2,ta)=[Real])
                        then [Bool]
                        else
                                raise typeException
	| hasType(Abs.binAnd(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Bool] andalso hasType(e2,ta)=[Bool])
                then [Bool]
                else
			raise typeException
	| hasType(Abs.binOr(e1,e2), ta:typeAssumption) =
                if (hasType(e1,ta)=[Bool] andalso hasType(e2,ta)=[Bool])
                then [Bool]
                else
                        raise typeException
	| hasType(Abs.not(e1), ta:typeAssumption) =
                if (hasType(e1,ta)=[Bool])
                then [Bool]
                else
                        raise typeException
	| hasType(Abs.proj(t,e), ta:typeAssumption) = 
		let
			fun extract (i,l) = if i=1 then hd(l) else extract(i-1,tl(l))
		in [extract(stringtoint(e),hasTypeTuple(t,ta))]
		end
	| hasType (Abs.expr(e), ta:typeAssumption) = hasType(e,ta)
	| hasType (Abs.ifthenelse(e1,e2,e3), ta:typeAssumption) = 
		if ( hasType(e1,ta)=[Bool] andalso hasType(e2,ta)=hasType(e3,ta) )
		then hasType(e2,ta)
		else raise typeException
	| hasType (Abs.function(v,e), ta:typeAssumption) = 
		let
			val t1 = hasType(v,ta);
			val t2 = hasType(e,([(v,t1)]@ta):typeAssumption );
		in t1@t2
		end
	| hasType (Abs.funApply(e1,e2), ta:typeAssumption) =
		hasType(e1,ta)
	| hasType (Abs.letinend(e1,e2,e3), ta: typeAssumption) = 
		hasType(e3,ta)
and hasTypeTuple(Abs.e(ex), ta:typeAssumption) = hasType(ex,ta)
	| hasTypeTuple(Abs.etuple(ex,t), ta:typeAssumption) = hasType(ex,ta) @ hasTypeTuple(t,ta)  


fun eval (Abs.variable(v),t:table) = valAssign(Abs.variable(v),t)
        | eval(Abs.tuple(tup), t:table) =  evalTuple(tup,t)
	| eval(Abs.proj(tup,e), t:table) = 
		let
			fun extract (i,l) = if i=1 then hd(l) else extract(i-1,tl(l))
		in [extract(stringtoint(e),evalTuple(tup,t))]
		end
	| eval(Abs.letinend(e1,e2,e3), t:table) = eval(e3,(e1,eval(e2,t))::t)
	| eval(Abs.function(v,e),t:table) = nil
	| eval(Abs.funApply(Abs.function(v,e),e2),t:table) = 
		eval(e,(v,eval(e2,t))::t)
        | eval(Abs.empty,t:table) =  []
        | eval(Abs.true,t:table) = [boolean(true)]
        | eval(Abs.false,t:table) = [boolean(false)]
        | eval(Abs.n(e),t:table) = [integer(stringtoint(e))]
        | eval(Abs.r(e),t:table) = [realNum(stringtoreal(e))]
	| eval(Abs.ifthenelse(e1,e2,e3),t:table) = 
		let
			fun reveal (boolean(a)) = a
				| reveal(_) = raise typeException
		in if reveal(hd(eval(e1,t))) then eval(e2,t) else eval(e3,t) 
		end
	| eval(Abs.neg(e1),t:table) =
                let
                        exception negConstituentException;
                        fun negAnsConstituent(integer(e1)) = integer(~e1)
                                | negAnsConstituent (realNum(e1)) = realNum(~e1)
                                | negAnsConstituent (_) = raise negConstituentException;
                in [negAnsConstituent((hd(eval(e1,t))))]
                end

        | eval(Abs.add(e1,e2),t:table) = 
		let
			exception addConstituentException;
			fun addAnsConstituent(integer(e1),integer(e2)) = integer(e1+e2)
        			| addAnsConstituent (realNum(e1), realNum(e2)) = realNum(e1+e2)
        			| addAnsConstituent (_,_) = raise addConstituentException;
		in [addAnsConstituent((hd(eval(e1,t))),(hd(eval(e2,t))))]
		end
	| eval(Abs.sub(e1,e2),t:table) =
                let
                        exception subConstituentException;
                        fun subAnsConstituent(integer(e1),integer(e2)) = integer(e1-e2)
                                | subAnsConstituent (realNum(e1), realNum(e2)) = realNum(e1-e2)
                                | subAnsConstituent (_,_) = raise subConstituentException;
                in [subAnsConstituent((hd(eval(e1,t))),(hd(eval(e2,t))))]
                end
	| eval(Abs.mul(e1,e2),t:table) =
                let
                        exception mulConstituentException;
                        fun mulAnsConstituent(integer(e1),integer(e2)) = integer(e1*e2)
                                | mulAnsConstituent (realNum(e1), realNum(e2)) = realNum(e1*e2)
                                | mulAnsConstituent (_,_) = raise mulConstituentException;
                in [mulAnsConstituent((hd(eval(e1,t))),(hd(eval(e2,t))))]
                end
	| eval(Abs.divide(e1,e2),t:table) =
                let
                        exception divideConstituentException;
                        fun divAnsConstituent(integer(e1),integer(e2)) = integer(e1 div e2)
                                | divAnsConstituent (realNum(e1), realNum(e2)) = realNum(e1/e2)
                                | divAnsConstituent (_,_) = raise divideConstituentException;
                in [divAnsConstituent((hd(eval(e1,t))),(hd(eval(e2,t))))]
                end
	| eval(Abs.gt(e1,e2),t:table) =
		let 
			exception compConstituentException;
			fun compAnsConstituent(integer(e1),integer(e2)) = boolean(e1>e2)
				| compAnsConstituent(realNum(e1),realNum(e2)) = boolean(e1>e2)
				| compAnsConstituent(_,_) = raise compConstituentException;
		in [compAnsConstituent(hd(eval(e1,t)),hd(eval(e2,t)))]
		end
	| eval(Abs.gte(e1,e2),t:table) = 
                let 
                        exception compConstituentException;
                        fun compAnsConstituent(integer(e1),integer(e2)) = boolean(e1>=e2)
                                | compAnsConstituent(realNum(e1),realNum(e2)) = boolean(e1>=e2)
                                | compAnsConstituent(_,_) = raise compConstituentException;
                in [compAnsConstituent(hd(eval(e1,t)),hd(eval(e2,t)))]
                end
	| eval(Abs.lt(e1,e2),t:table) = 
                let 
                        exception compConstituentException;
                        fun compAnsConstituent(integer(e1),integer(e2)) = boolean(e1<e2)
                                | compAnsConstituent(realNum(e1),realNum(e2)) = boolean(e1<e2)
                                | compAnsConstituent(_,_) = raise compConstituentException;
                in [compAnsConstituent(hd(eval(e1,t)),hd(eval(e2,t)))]
                end
	| eval(Abs.lte(e1,e2),t:table) = 
                let 
                        exception compConstituentException;
                        fun compAnsConstituent(integer(e1),integer(e2)) = boolean(e1<=e2)
                                | compAnsConstituent(realNum(e1),realNum(e2)) = boolean(e1<=e2)
                                | compAnsConstituent(_,_) = raise compConstituentException;
                in [compAnsConstituent(hd(eval(e1,t)),hd(eval(e2,t)))]
                end
	| eval(Abs.equals(e1,e2),t:table) = 
                let 
                        exception compConstituentException;
                        fun compAnsConstituent(integer(e1),integer(e2)) = boolean(e1=e2)
                                | compAnsConstituent(_,_) = raise compConstituentException;
                in [compAnsConstituent(hd(eval(e1,t)),hd(eval(e2,t)))]
                end
	| eval(Abs.binAnd(e1,e2),t:table) = 
                let 
                        exception andConstituentException;
                        fun andAnsConstituent(boolean(e1),boolean(e2)) = boolean(e1 andalso e2)
                                | andAnsConstituent(_,_) = raise andConstituentException;
                in [andAnsConstituent(hd(eval(e1,t)),hd(eval(e2,t)))]
                end
	| eval(Abs.binOr(e1,e2),t:table) =
                let 
                        exception orConstituentException;
                        fun orAnsConstituent(boolean(e1),boolean(e2)) = boolean(e1 orelse e2)
                                | orAnsConstituent(_,_) = raise orConstituentException;
                in [orAnsConstituent(hd(eval(e1,t)),hd(eval(e2,t)))]
                end
	| eval(Abs.not(e1),t:table) =
                let 
                        exception notConstituentException;
                        fun notAnsConstituent(boolean(e1)) = boolean(not(e1))
                                | notAnsConstituent(_) = raise notConstituentException;
                in [notAnsConstituent(hd(eval(e1,t)))]
                end
	| eval(Abs.expr(e), t:table) = eval(e,t)

and evalTuple(Abs.e(ex), t:table) = eval(ex,t)
        | evalTuple(Abs.etuple(ex,tup), t:table) = eval(ex,t) @ evalTuple(tup,t)


