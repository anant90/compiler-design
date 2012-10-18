(*unifier.sml*)
(*Anant Jain, 2008EE10330, Group 2*)

(* 1a. Devise and implement a representation for an arbitrary signature.*)
type sign = (string * int) list;

(* 1b. For any signature, implement a datatype for terms over that signature, and write functions to check that terms are well formed, a function that gives the size and height of terms, a function that returns the set of variables appearing in the term and any other such useful functions on terms. *)

datatype termConstituent = 
	Name of string
	| Arguments of ((termConstituent list) list);

type term = termConstituent list;

(* Assumption: All variables start with a capital letter, non variables with a small *)

fun isVariable str = ((hd(explode str) >= #"A") andalso (hd(explode str) <= #"Z"));

(* A valid signature has no variables *)
fun isValidSign [] = true
| isValidSign ((n,a)::tl) = (not (isVariable n) andalso (isValidSign tl));


exception notValidSymbol;
(*find arity of a symbol named str, arity of variables is returned 0 *)
fun arity ((n,a)::tl) str = 
	if str = n
	then a
	else arity tl str
| arity _ str = if isVariable str
	then 0
	else raise notValidSymbol;

(*Define a helper function forall *)
fun forall prop (hd::tl) = (prop hd) andalso (forall prop tl)
  | forall prop [] = true;

(*Find if the terms are well formed, given a signature and a term *)
fun isValidTerm sign [Name s1, Arguments l1] =
        ((arity sign s1) = length(l1) andalso
	(forall (fn x => (isValidTerm sign x)) l1))
  | isValidTerm sign [Name s1]= ((arity sign s1) = 0)
  | isValidTerm _ _ = false;

fun max a b = if a>b then a else b;
fun maxhelper(a,[]) = a 
  | maxhelper(a,hd::tl) = if (hd>a) then maxhelper(hd,tl) else maxhelper(a,tl);
fun maxOfList [] = 0
  | maxOfList (hd::tl) = maxhelper(hd,tl);

(*Find height of a term*)
fun heightOfTerm [] = 0
  | heightOfTerm[Name s1, Arguments l1] = 
	1+maxOfList(map heightOfTerm l1);

fun sumOfList [] = 0
  | sumOfList (hd::tl) = hd+sumOfList(tl);
(*Find size of term *)
fun sizeOfTerm [] = 0
  | sizeOfTerm [Name s1, Arguments l1] = 
	1 + sumOfList(map sizeOfTerm l1);


(*2. a. Device and represent efficiently the concept of substitutions.*)
(*b. Write a function subst that given a term and a substitution, applied the substitution to the term. *)

exception SubstError;
(* subst givenTerm var t: substitute t for var in givenTerm *)
fun subst [Name st1] [Name st2] t = (*Variable for Variable*)
        if (st1 = st2) then t else [Name st1]
  | subst [Name F, Arguments al] v t =
        [Name F, Arguments (map (fn x => (subst x v t)) al)]
  | subst _ _ _ = raise SubstError;

fun exists prop (hd::tl) =
                (prop hd) orelse (exists prop tl)
                | exists prop [] = false;

exception vITException;
fun varInTree [Name st1] [Name st2] = 
	if isVariable st1 then (st1=st2) else raise vITException
	| varInTree V [Name F, Arguments Al] =  exists (fn x => varInTree V x) Al
	| varInTree _ _ = raise vITException;

fun isInVars var [] = false
	| isInVars var ((v,term)::ls) = (v=var) orelse (varInTree var term) orelse isInVars var ls;

fun substitute (var, term, []) = []
	| substitute (var, term, (([Name s1],expr)::ls)) = [([Name s1], (subst expr var term))]@(substitute(var, term, ls));
(*Composition of substitutions *)
fun composeActual (s,([Name v],term)) = 
	if (isInVars [Name v] s) 
	then (substitute ([Name v], term, s))
	else s@[([Name v],term)];
 
fun compose (a,[])  = a
	| compose (a,(x::ls)) = compose (composeActual(a,x), ls);


(* 3. Implement a function unif, that given two terms t1 and t2, computes their most general unifier, if it exists. *)

exception mguSearchFailed;

fun makePair (h1::l1) (h2::l2) = (h1,h2)::(makePair l1 l2)
  | makePair [] [] = [];

exception notAValidArg;
(* take one step in finding the mgu *)
fun mgustep ([Name F1, Arguments al1],[Name F2, Arguments al2]) done remaining =
	if F1 = F2
	then (done, (makePair al1 al2) @ remaining)
	else raise mguSearchFailed

  | mgustep ([Name st1], [Name st2]) done remaining =
    let
        val t1 = [Name st1];
        val t2 = [Name st2];
    in
        if (st1 = st2)
        then (done, remaining)
        else
            if isVariable st1
            then
                ((t1, t2)::done,
                 (map (fn (ta,tb) => (subst ta t1 t2,
                                      subst tb t1 t2)) remaining))
            else
                if isVariable st2
                then (done, (t2,t1)::remaining)
                else raise mguSearchFailed
    end

  | mgustep ([Name st1], t) done remaining =
    let
        val t1 = [Name st1];
	fun exists prop (hd::tl) = 
		(prop hd) orelse (exists prop tl)
 		| exists prop [] = false;

	fun varInTree [Name st1] [Name st2] = 
		if isVariable st1 then (st1=st2) else raise notAValidArg
	| varInTree V [Name F, Arguments Al] =  exists (fn x => varInTree V x) Al
  	| varInTree _ _ = raise notAValidArg ;
    in
        if varInTree t1 t (*Ensure st1 variable is not present in the tree of t *)
        then raise mguSearchFailed
        else ((t1, t)::done, map (fn (ta, tb) => (subst ta t1 t,subst tb t1 t)) remaining)
    end
  | mgustep (t, [Name st1]) done remaining = (done, ([Name st1], t)::remaining)
  | mgustep _ _ _ = raise mguSearchFailed;

fun iter (done, (hd::tl)) = mgustep hd done tl;
fun hasFinished (done, []) = true
  | hasFinished _ = false;

(* A helper function to find a term v in a list of terms and return the arguments part*)
exception notFound;
fun findTerm v ((lh1,lh2)::tl) = 
	if v = lh1 then lh2
	else findTerm v tl
| findTerm v [] = raise notFound;

(* A function to determine whether a symbol is a variable or not *)
fun isVar [Name st] = isVariable st
  | isVar _ = false;

(*A helper function to allow for substitution of variables by variables*)
fun helper (lh,rh) done (hd::tl) =
	if isVar rh
	then
	helper hd ((lh,(findTerm rh (done@(hd::tl))))::done) tl
	handle notFound => helper hd ((lh,rh)::done) tl
                else helper hd ((lh,rh)::done) tl
  | helper(lh,rh) done [] =
	if isVar rh
	then 
	((lh,(findTerm rh done))::done)
	handle notFound => ((lh,rh)::done)
                else ((lh,rh)::done);

fun solve sol = 
	let
		val (hdsol::tlsol) = sol
	in
		helper hdsol [] tlsol
	end;

(* iterate applying f until stopcond is true *)
fun iterate f stopCond x = if (stopCond x)
                          then x
                          else iterate f stopCond (f x);

(* function to find the mgu of two terms t1 and t2 *)
fun findmgu1 t1 t2 = solve (#1 (iterate iter hasFinished ([],[(t1,t2)])));

fun applyComposition (a,[]) = a
	| applyComposition (a, x::xs) = applyComposition(compose(a,x),xs); 
fun findmgu t1 t2 = applyComposition([],[(findmgu1 t1 t2)]);

