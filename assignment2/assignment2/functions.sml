Control.Print.printDepth := 20;

fun max(a,b) = if a>b then a else b;

fun heightOfProgram(Abs.empty) = 0
	| heightOfProgram(Abs.program(a,b)) = 1+max(hS(a),heightOfProgram(b))
and hS(Abs.fact(f)) = 1+hF(f) 
	| hS(Abs.rule(r)) = 1+hR(r)
and hF(Abs.literal(l)) = 1+hL(l)
and hR(Abs.headbody(h,b)) = 1+max(hH(h), hB(b))
and hH(Abs.head(h)) = 1+hL(h)
and hB(Abs.body(b)) = 1+hL(b)
	| hB(Abs.literalbody(l,b)) = 1+max(hL(l),hB(b))
and hL(Abs.lname(n)) = 1
	| hL(Abs.nametuple(n,t)) = 1+hTuple(t)
and hTuple(Abs.tuple(t)) = 1+hTerms(t)
and hTerms(Abs.term(t)) = 1+hTerm(t)
	| hTerms(Abs.terms(t,tt)) = 1+max(hTerm(t),hTerms(tt)) 
and hTerm(Abs.tname(n)) = 1
	| hTerm(Abs.tvar(n)) = 1
	| hTerm(Abs.ttot(n,t)) = 1+hTuple(t)

fun add (a,b) = a+b;
fun sizeOfProgram(Abs.empty) = 0
       | sizeOfProgram(Abs.program(a,b)) = 1+add(sS(a),sizeOfProgram(b))
and sS(Abs.fact(f)) = 1+sF(f)
        | sS(Abs.rule(r)) = 1+sR(r)
and sF(Abs.literal(l)) = 1+sL(l)
and sR(Abs.headbody(h,b)) = 1+add(sH(h), sB(b))
and sH(Abs.head(h)) = 1+sL(h)
and sB(Abs.body(b)) = 1+sL(b)
        | sB(Abs.literalbody(l,b)) = 1+add(sL(l),sB(b))
and sL(Abs.lname(n)) = 2
        | sL(Abs.nametuple(n,t)) = 2+sTuple(t)
and sTuple(Abs.tuple(t)) = 1+sTerms(t)
and sTerms(Abs.term(t)) = 1+sTerm(t) 
        | sTerms(Abs.terms(t,tt)) = 1+max(sTerm(t),sTerms(tt))
and sTerm(Abs.tname(n)) = 2
        | sTerm(Abs.tvar(n)) = 2
        | sTerm(Abs.ttot(n,t)) = 2+sTuple(t)

fun append(a,b) = a@b;
fun listOfProgram(Abs.empty) = []
        | listOfProgram(Abs.program(a,b)) = append(lS(a),listOfProgram(b))
and lS(Abs.fact(f)) = lF(f)
        | lS(Abs.rule(r)) = lR(r)
and lF(Abs.literal(l)) = lL(l)
and lR(Abs.headbody(h,b)) = append(lH(h), lB(b))
and lH(Abs.head(h)) = lL(h)
and lB(Abs.body(b)) = lL(b)
        | lB(Abs.literalbody(l,b)) = append(lL(l),lB(b))
and lL(Abs.lname(n)) = []
        | lL(Abs.nametuple(n,t)) = lTuple(t)
and lTuple(Abs.tuple(t)) = lTerms(t)
and lTerms(Abs.term(t)) = lTerm(t)
        | lTerms(Abs.terms(t,tt)) = append(lTerm(t),lTerms(tt))
and lTerm(Abs.tname(n)) = []
        | lTerm(Abs.tvar(n)) = n::[]
	| lTerm(Abs.ttot(n,t)) = lTuple(t)


