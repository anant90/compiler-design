(* secd.sml *)
(* Anant Jain, 2008EE10330, Group 2 *)

type var = string

datatype operator = PLUS | TIMES | MINUS | EQUAL | LESS | AND


datatype exp
  = Var	of var
  | Num	of int
  | Bool of bool
  | Prim of (operator *	exp * exp)
  | If of (exp * exp * exp)
  | Fn of (var * bindpat * exp)
  | Apply of (exp * exp)
  | Let of (bindpat * exp * exp)
  | Tuple of exp list
  | Proj of exp list * exp
and bindpat
  = AtomicPat of var
  | TuplePat of var list

datatype instruction
  = INum of int          (* push integer constant on stack *)
  | IBool of bool        (* push boolean constant on stack *)
  | IVar of int          (* fetch i-th element of environment stack *)
  | IPrim of operator    (* apply operator to top 2 stack elements *)
  | IApp                 (* apply function to argument *)
  | IIf of code * code   (* eval then or else branch of an If *)
  | IFn of code          (* create a closure *)
  | IBind of int         (* bind the definients value of a let *)
  | IUnbind of int       (* unbind let definiens after body evaluated *)
  | ITup of int          (* construct tuple from top n values on stack *)
  | IDestTup             (* destruct tuple value on top of stack *)
withtype code = instruction list
  

datatype value
  = NUM of int
  | BOOL of bool
  | CLSR of code * runEnv   (* closure, i.e. function value *)
     (* [fn f(x) => e; E] => CLSR(code(f::x::E, e), E) *)
  | TUP of value list      (* tuple values *)
withtype runEnv = value list

type stack = value list

type control = code

type state = (stack * runEnv * control) list

(*The compiler environment*)
type compEnv = var list   

exception CompError

fun extract(n,L: 'a list) = if n=1 then hd(L) else extract(n-1, tl(L));

(* position : var * Clo list -> int *)
fun position(v,E) =
    let fun pos (v, nil, n) =
	    (print ("unbound var: "^v^"\n"); raise CompError)
	  | pos (v, v'::E, n) = if v = v' then n else pos(v,E,n+1)
     in pos(v,E,0)
    end

exception RunError of string
(* lookup : Clo list * int -> Clo *)
fun lookup(E,n) = List.nth(E,n)
                  handle Subscript => raise RunError "lookup"


(* compile: compEnv * exp -> code
   simple compiler translating MinML expression to rpn code *)
fun compile(E, Var v) = [IVar (position(v,E))]
  | compile(E, Num n) = [INum n]
  | compile(E, Bool b) = [IBool b]
  | compile(E, Fn(f,pat,e)) = 
      (case pat
	 of AtomicPat x =>
	    [IFn(compile(f::x::E, e))]
	  | TuplePat xs =>
	    [IFn(IVar 1 :: IDestTup :: IBind (length xs) :: compile(xs@(f::""::E), e))])
  | compile(E, Prim(oper,e1,e2)) =
      (compile(E,e1))@(compile(E,e2))@[IPrim(oper)]
  | compile(E, Apply(e1,e2)) =
      (compile(E,e1))@(compile(E,e2))@[IApp]
  | compile(E, If(e,e1,e2)) = 
      (compile(E,e))@[IIf(compile(E,e1), compile(E,e2))]
  | compile(E, Proj(es:exp list,Var index)) = compile(E,extract(1,es))
  | compile(E, Let(pat,e1,e2)) =
      (case pat
         of AtomicPat x => 
	    compile(E,e1)@(IBind 1 :: compile(x::E,e2)@[IUnbind 1])
	  | TuplePat xs =>  (* length xs > 1 *)
	    let val n = length xs
	     in compile(E,e1)@(IDestTup :: IBind n :: compile(xs@E,e2)@[IUnbind n])
	    end)
  | compile(E, Tuple(es)) =
     (foldr (fn (e,code) => compile(E,e)@code) [] es)@[ITup (length es)]
	


(* operations on the runtime environment *)


(* bind : runEnv * value -> runEnv *)
fun bind(E,v) = v::E


(* primEval: operation * value * value -> value
 *   evaluation of applications of primitive operations *)
fun primEval(PLUS, NUM n1, NUM n2) = NUM(n1 + n2)
  | primEval(TIMES, NUM n1, NUM n2) = NUM(n1 * n2)
  | primEval(MINUS, NUM n1, NUM n2) = NUM(n1 - n2)
  | primEval(EQUAL, NUM n1, NUM n2) = BOOL(n1 = n2)
  | primEval(LESS, NUM n1, NUM n2) = BOOL(n1 < n2)
  | primEval(AND, BOOL n1, BOOL n2) = BOOL(n1 andalso n2)
  | primEval _ = raise Fail "primEval"


fun steps([(v::_, _, nil)]) = v  (* completion *)

  | steps((S, E, (INum n)::C)::D) =    (* int constant *)
      steps(((NUM n)::S, E, C)::D)

  | steps((S, E, (IBool b)::C)::D) =   (* bool constant *)
      steps(((BOOL b)::S, E, C)::D)

  | steps((S, E, (IVar n)::C)::D) =    (* variable *)
      steps((lookup(E,n)::S, E, C)::D)

  | steps((S, E, IFn(c)::C)::D) =    (* create function closure *)
      steps((CLSR(c,E)::S, E, C)::D)

  | steps((v2::v1::S, E, IPrim(oper)::C)::D) =     (* apply primitive op oper *)
      steps((primEval(oper,v1,v2)::S, E, C)::D)

  | steps((v2::(v1 as CLSR(c,E'))::S, E, IApp::C)::D) =  (* function call *) 
      steps((nil, bind(bind(E',v2),v1), c)::(S,E,C)::D)

  | steps((v::_, _, nil)::(S,E,C)::D) =    (* function return *)
      steps((v::S, E, C)::D)

  | steps(((BOOL true)::S, E, IIf(cthen,celse)::C)::D) =
      steps((S, E, cthen@C)::D)    (* conditional branch, true case *)

  | steps(((BOOL false)::S, E, IIf(cthen,celse)::C)::D) =
      steps((S, E, celse@C)::D)    (* conditional branch, false case *)

  (* binding the value(s) of a Let-definiens *)
  | steps((S, E, IBind n::C)::D) =
      let val vs = List.take(S, n)
	  val S' = List.drop(S, n)
	  val E' = rev vs @ E
      in steps((S', E', C)::D)
      end

  (* unbinding the value of a Let-definiens after Let-body is evaluated  *)
  | steps((S, E, IUnbind n::C)::D) =
      steps((S, List.drop(E,n), C)::D)

  (* unbinding the value of a Let-definiens after Let-body is evaluated  *)
  | steps((S, E, ITup n :: C)::D) =
      let val S' = TUP(rev(List.take(S,n)))::(List.drop(S,n))
       in steps((S', E, C)::D)
      end

  (* destructing a tuple value *)
  | steps((TUP vs :: S, E, IDestTup :: C) :: D) =
      steps((rev vs @ S, E, C) :: D)

  | steps _ = raise RunError "bad state"

fun init (e:exp): state = [(nil,nil,(compile(nil,e)))]

fun eval (e: exp) : value = steps(init e);

