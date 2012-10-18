(* imper.sml *)
(* Anant Jain, 2008EE10330, Group 2 *)
Control.Print.printDepth := 100;

type variable = string * int
type localVars = variable list
type params = variable list
type procName = string

type proc = string  * params * localVars * procName list * procName
(* Procedure name, Parameters, Local Variables, Nested procedures, Parent procedure *)

val V:proc = ("V", [("b",0),("c",0)], [("z",0)],[], "R")
val R:proc = ("R", [("a",0),("x",0)], [("c",0),("w",0)], ["V"], "P")
val S:proc = ("S", [("y",0),("w",0)], [("s",0),("t",0),("u",0)], [], "P")
val P:proc = ("P", [("x",0),("y",0)], [("a",0),("z",0)], ["R","S"], "main")
val main:proc = ("main",[],[("a",0),("b",0),("c",0)],["P","Q"],"")
val Q:proc = ("Q", [("x",0),("z",0)], [("a",0),("b",0),("y",0)], ["T","U"], "main")
val T:proc = ("T", [("x",0)], [("a",0),("c",0)], ["W"], "Q")
val W:proc = ("W", [("v",0)], [("s",0),("t",0)], [], "T")
val U:proc = ("U", [("y",0),("z",0)], [("u",0),("v",0)], [], "Q")

val listOfProc = [main,P,Q,R,S,T,U,V,W];
(*Note that we could make the linking implicit in the sturcture of the procedures themselves, leading to huge one DS of the tree, but it would've been inconvenient while pushing individual procedures on the call stack etc. Thus let this listOfProc be the bad guy - the complex uniting list of the tree, wherein the elements know the inherent hierarchy *)

(* Execution starts here *)
(* initialize call stack by calling main*)
val cS = [main]

(* Given a procedure, extract its name *)
fun procedureName(p:proc) =
        let val (n,_,_,_,_) = p
        in n
        end

fun procedureParentName(p:proc) =
        let val (_,_,_,_,n) = p
        in n
        end

exception procedureSearchFailed;
fun procedureSearch ([], _) = raise procedureSearchFailed
  | procedureSearch(head::tail, procName) = 
	if procedureName(head) = procName then head else procedureSearch(tail, procName)

(*0. Display the current call stack in terms of procedures whose frames are on stack.*)

fun displayCallStack(nil) = nil
 | displayCallStack(hd::tl) = [procedureName(hd)]@displayCallStack(tl)

(*Test: *) 
val ans0 = displayCallStack(cS);

fun belongsTo (a,[]) = false
  | belongsTo (a,l::ls) = if a=l then true else belongsTo(a,ls);

fun splUnion ([],b) = b
  | splUnion (a::tlA, b) = 
	if belongsTo(a,b) then splUnion(tlA,b) else splUnion(tlA, a::b); 

(* 1. Display all the variables that can be read in the current procedure's body, and their current values *)
fun displayAllVars1("", answerList,_) = answerList
  | displayAllVars1(currentProcName,answerList,cS) = 
	let 
		val currentProc = procedureSearch(cS,currentProcName);
		val (name,arguments,locals,_,parent) = currentProc;
		val answer = splUnion(locals, answerList);
		val answer = splUnion(arguments, answer);
	in
		displayAllVars1(procedureParentName(currentProc),answer,cS)
	end;		

fun displayAllVars(cS) = displayAllVars1(procedureName(hd(cS)),nil,cS);

val ans1 = displayAllVars(cS);

(* 2. Change the value of a given variable. *)

fun assignVal (a,[],value,ans) = ans
  | assignVal (a,(l,v)::ls,value,ans) = if a=l then ans@[(a,value)]@ls else assignVal(a,ls,value,ans@[(l,v)]);

fun belongsTo1 (a,[]) = false
  | belongsTo1 (a,(l,v)::ls) = if a=l then true else belongsTo1(a,ls);

fun assignVar1([],_,_,_,ans) = ans
  | assignVar1(cShead::cStail, varName:string, newVal:int, parentName,ans) =
	if(procedureName(cShead)) = parentName then
		let
			val (name,arguments,locals,children,parent) = cShead;
			val x = if(belongsTo1(varName,locals)) then ans@[(name,arguments,assignVal(varName,locals,newVal,nil),children,parent)]@cStail
			else if (belongsTo1(varName,arguments)) then ans@[(name,arguments,assignVal(varName,arguments,newVal,nil),children,parent)]@cStail
			else assignVar1(cStail, varName, newVal, procedureParentName(cShead),cShead::ans)
		in x
		end
	else assignVar1(cStail,varName, newVal, parentName,cShead::ans);

fun assignVar (varName:string, newVal:int,cS) = assignVar1(cS, varName, newVal, procedureName(hd(cS)), nil)

val ans2 = assignVar ("a",5,cS); 

(*3. Display the procedures that can be called from the current procedure. *)
(* First child of any ancestor is callable *)

fun callable (_, "") = ["main"]
	| callable (listOfProc, callerName) = 
	if procedureName(hd(listOfProc))=callerName 
	then 
	(*give the list of all the children and do callable on the ancestor*)
	let val (_,_,_,children,parent) = hd(listOfProc) in children@callable(listOfProc,parent) end
	else callable (tl(listOfProc),callerName) 

val currentProcName = procedureName(hd(cS));
val ans3 = callable (listOfProc, currentProcName)

(* 4. Call a particular procedure from one of the callable procedures (others should not be callable). *)

exception functionNotCallable;

fun assignValues([],_,ans) = ans 
  | assignValues((a,v)::ls, value::ls1, ans) = assignValues(ls,ls1,ans@[(a,value)]);

fun call1(listOfProc, cS, procName,params:int list) =
	if belongsTo(procName,callable(listOfProc, procedureName(hd(cS)))) then
	let
		val (n,arg,loc,c,p) = procedureSearch(listOfProc,procName);
		val arguments = assignValues(arg,params,nil);
	in (n,arguments,loc,c,p)::cS
	end
	else raise functionNotCallable;

fun call (funName, params) = call1(listOfProc, cS, funName, params);

val cS = call("P",[1,2]);

(* 5. Return from the current procedure.*)

fun return (cS) = tl(cS);

val ans5 = return(cS);

(* model the call stack, dummy frames created and placed on the stack, and the display records at any point in the execution *)
