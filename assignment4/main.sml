use "functions.sml";
use "unifier.sml";

val q = Logic.compile("qinput2.pl");
val d = Logic.compile("input2.pl");
fun append(a,b) = a@b

fun reform (hd::tl) = [[hd]]@reform(tl)
	| reform ([]) = [[]];

fun removeLast (hd::tl,ans) = if tl=nil then ans else removeLast(tl,ans@[hd]);

(* Compile a Abs.program into a list of facts and rules *)

fun compileProgram(Abs.empty) = []
        | compileProgram(Abs.program(a,b)) = append(lS(a),compileProgram(b))
and lS(Abs.fact(f)) = lF(f)
        | lS(Abs.rule(r)) = lR(r)
and lF(Abs.literal(l)) = [(lL(l),[])]
and lR(Abs.headbody(h,b)) = [(lH(h), lB(b))]
and lH(Abs.head(h)) = lL(h)
and lB(Abs.body(b)) = lL(b)
        | lB(Abs.literalbody(l,b)) = append(lL(l),lB(b))
and lL(Abs.lname(n)) = [Name n]
        | lL(Abs.nametuple(n,t)) = [Name n, Arguments (removeLast(reform(lTuple(t)),[])) ]
and lTuple(Abs.tuple(t)) = lTerms(t)
and lTerms(Abs.term(t)) = lTerm(t)
        | lTerms(Abs.terms(t,tt)) = append(lTerm(t),lTerms(tt))
and lTerm(Abs.tname(n)) = [Name n]
        | lTerm(Abs.tvar(n)) = [Name n]
        | lTerm(Abs.ttot(n,t)) = [Name n, Arguments [lTuple(t)]]

val database = compileProgram d;
val query = compileProgram q;

fun applyMgu (givenTerm,[]) = givenTerm
  | applyMgu (givenTerm,mguList) = 
	let val (v,t)::remainingMguList = mguList;
	in applyMgu (subst givenTerm v t,remainingMguList)
  	end;

fun applyMguList ([],mgu) = [[]]
  | applyMguList (l,mgu) = 
	let
		val n::a::tail = l
	in
		applyMgu([n]@[a],mgu)::applyMguList(tail,mgu)
	end;

fun applyMguListSpl ([],mgu) = [[]]
  | applyMguListSpl ([l],mgu) =
        let
                val n::a::tail = l
        in
                applyMgu([n]@[a],mgu)::applyMguListSpl([tail],mgu)
        end
  | applyMguListSpl (_,mgu) = [[]];


fun head [] = []
	| head a = hd(a);
fun sA ([[]],a) = a
  | sA (a,b) = a@b;

fun stackUp (_,[],r,_,m) = [(m,false,r)]
  | stackUp ([[]],_,r,_,m) = [(m,true,r)]
  | stackUp ([[],[]],_,r,_,m) = [(m,true,r)]
  | stackUp (sGL, db,n,database,m) = 
	let
		val a::b = sGL;
		val (h,body) = hd(db);
		val mgu = findmgu a h;
	in
		stackUp(sA((applyMguList(body,mgu)),(applyMguList(head(b),mgu))),database,n+1,database,compose(m,mgu))@stackUp(sGL,tl(db),n+1,database,m)
		(*stackUp(applyMguList(body,mgu),database,n+1,database,compose(m,mgu))@stackUp(sGL,tl(db),n+1,database,m) *)
	end 
	handle mguSearchFailed => stackUp(sGL,tl(db),n+1,database,m)

val (q,waste) = hd(query);

val ans = stackUp([q],database,0,database,[])


(*
(*Iteration 1*)
val sGL = [q];
val db = database;
val m = [];
val a::b = sGL;
val (h,body) = hd(db); 
(*Failed mgu - first edge *)

val db = tl(db);
val (h,body) = hd(db);
(*Failed mgu - second edge *)

val db = tl(db);
val a::b = sGL;
val (h,body) = hd(db);
val mgu = findmgu a h;

val p1 = (applyMguList(body,mgu))
val p2 = (applyMguListSpl(b,mgu))
val p = p1@p2;

(*Carrying out the second subsequent stackup call *)
(*stackUp(sGL,tl(db),0,database,m);*)

(*Iteration 1*)
val db = tl(db);
val a::b = sGL;
val (h,body) = hd(db);
val mgu = findmgu a h;

val p1 = (applyMguList(body,mgu))
val p2 = (applyMguListSpl(b,mgu))
val p = p1@p2;


(*In second, Carrying out the fisrt subsequent stackUp call *)
val sGL = p;
val db = database;
val m = m@mgu;
val a::b = sGL;
val (h,body) = hd(db);
val mgu = findmgu a h;


val p1 = (applyMguList(body,mgu))
val p2 = (applyMguList(head(b),mgu))
val p = sA(p1,p2);

(*
val p1 = (applyMguList(body,mgu))
val p2 = (applyMguListSpl(b,mgu))
val p = p1@p2
*)
*)



