
functor LogicLrValsFun (structure Token : TOKEN
                               structure Abs : ABS ):Logic_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct
open Abs

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\086\000\007\000\101\000\016\000\035\000\017\000\034\000\
\\018\000\033\000\019\000\032\000\023\000\031\000\027\000\101\000\000\000\
\\001\000\002\000\093\000\003\000\093\000\004\000\093\000\006\000\026\000\
\\007\000\025\000\008\000\024\000\009\000\023\000\010\000\022\000\
\\011\000\021\000\016\000\113\000\017\000\113\000\018\000\113\000\
\\019\000\113\000\020\000\020\000\023\000\093\000\024\000\019\000\
\\025\000\093\000\026\000\018\000\028\000\093\000\029\000\017\000\
\\030\000\016\000\031\000\015\000\032\000\014\000\033\000\093\000\
\\034\000\093\000\035\000\093\000\036\000\013\000\037\000\093\000\
\\038\000\093\000\000\000\
\\001\000\002\000\093\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\010\000\022\000\011\000\021\000\016\000\113\000\
\\017\000\113\000\018\000\113\000\019\000\113\000\020\000\020\000\
\\023\000\113\000\024\000\019\000\026\000\018\000\027\000\113\000\
\\029\000\017\000\030\000\016\000\031\000\015\000\032\000\014\000\
\\036\000\013\000\000\000\
\\001\000\002\000\093\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\010\000\022\000\011\000\021\000\016\000\113\000\
\\017\000\113\000\018\000\113\000\019\000\113\000\020\000\020\000\
\\023\000\113\000\024\000\019\000\026\000\018\000\029\000\017\000\
\\030\000\016\000\031\000\015\000\032\000\014\000\036\000\013\000\000\000\
\\001\000\002\000\104\000\003\000\104\000\004\000\104\000\007\000\123\000\
\\012\000\123\000\013\000\123\000\014\000\123\000\015\000\123\000\
\\016\000\123\000\017\000\123\000\018\000\123\000\019\000\123\000\
\\021\000\104\000\022\000\104\000\023\000\104\000\025\000\104\000\
\\027\000\123\000\028\000\104\000\033\000\104\000\034\000\104\000\
\\035\000\104\000\037\000\104\000\038\000\104\000\000\000\
\\001\000\002\000\067\000\000\000\
\\001\000\003\000\093\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\010\000\022\000\011\000\021\000\016\000\113\000\
\\017\000\113\000\018\000\113\000\019\000\113\000\020\000\020\000\
\\023\000\113\000\024\000\019\000\026\000\018\000\029\000\017\000\
\\030\000\016\000\031\000\015\000\032\000\014\000\036\000\013\000\000\000\
\\001\000\003\000\065\000\000\000\
\\001\000\004\000\093\000\006\000\026\000\007\000\025\000\008\000\024\000\
\\009\000\023\000\010\000\022\000\011\000\021\000\016\000\113\000\
\\017\000\113\000\018\000\113\000\019\000\113\000\020\000\020\000\
\\023\000\113\000\024\000\019\000\026\000\018\000\029\000\017\000\
\\030\000\016\000\031\000\015\000\032\000\014\000\036\000\013\000\000\000\
\\001\000\004\000\064\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\016\000\113\000\017\000\113\000\
\\018\000\113\000\019\000\113\000\020\000\020\000\023\000\093\000\
\\024\000\019\000\026\000\018\000\029\000\017\000\030\000\016\000\
\\031\000\015\000\032\000\014\000\036\000\013\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\016\000\113\000\017\000\113\000\
\\018\000\113\000\019\000\113\000\020\000\020\000\023\000\113\000\
\\024\000\019\000\025\000\093\000\026\000\018\000\029\000\017\000\
\\030\000\016\000\031\000\015\000\032\000\014\000\036\000\013\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\016\000\113\000\017\000\113\000\
\\018\000\113\000\019\000\113\000\020\000\020\000\023\000\113\000\
\\024\000\019\000\026\000\018\000\028\000\093\000\029\000\017\000\
\\030\000\016\000\031\000\015\000\032\000\014\000\036\000\013\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\016\000\113\000\017\000\113\000\
\\018\000\113\000\019\000\113\000\020\000\020\000\023\000\113\000\
\\024\000\019\000\026\000\018\000\029\000\017\000\030\000\016\000\
\\031\000\015\000\032\000\014\000\033\000\093\000\036\000\013\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\016\000\113\000\017\000\113\000\
\\018\000\113\000\019\000\113\000\020\000\020\000\023\000\113\000\
\\024\000\019\000\026\000\018\000\029\000\017\000\030\000\016\000\
\\031\000\015\000\032\000\014\000\034\000\093\000\036\000\013\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\016\000\113\000\017\000\113\000\
\\018\000\113\000\019\000\113\000\020\000\020\000\023\000\113\000\
\\024\000\019\000\026\000\018\000\029\000\017\000\030\000\016\000\
\\031\000\015\000\032\000\014\000\035\000\093\000\036\000\013\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\016\000\113\000\017\000\113\000\
\\018\000\113\000\019\000\113\000\020\000\020\000\023\000\113\000\
\\024\000\019\000\026\000\018\000\029\000\017\000\030\000\016\000\
\\031\000\015\000\032\000\014\000\036\000\013\000\037\000\093\000\000\000\
\\001\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\016\000\113\000\017\000\113\000\
\\018\000\113\000\019\000\113\000\020\000\020\000\023\000\113\000\
\\024\000\019\000\026\000\018\000\029\000\017\000\030\000\016\000\
\\031\000\015\000\032\000\014\000\036\000\013\000\038\000\093\000\000\000\
\\001\000\006\000\049\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\024\000\019\000\000\000\
\\001\000\006\000\049\000\007\000\025\000\008\000\024\000\024\000\019\000\000\000\
\\001\000\007\000\066\000\000\000\
\\001\000\016\000\035\000\017\000\034\000\018\000\033\000\019\000\032\000\
\\023\000\031\000\000\000\
\\001\000\023\000\062\000\000\000\
\\001\000\024\000\047\000\000\000\
\\001\000\025\000\069\000\000\000\
\\001\000\025\000\076\000\000\000\
\\001\000\027\000\068\000\000\000\
\\001\000\028\000\000\000\000\000\
\\001\000\033\000\063\000\000\000\
\\001\000\034\000\078\000\000\000\
\\001\000\035\000\082\000\000\000\
\\001\000\037\000\077\000\000\000\
\\001\000\038\000\081\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\016\000\035\000\017\000\034\000\018\000\033\000\019\000\032\000\
\\023\000\031\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\094\000\022\000\037\000\000\000\
\\095\000\000\000\
\\096\000\021\000\036\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\012\000\030\000\013\000\029\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\006\000\026\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\010\000\022\000\011\000\021\000\020\000\020\000\024\000\019\000\000\000\
\\113\000\006\000\049\000\007\000\025\000\008\000\024\000\009\000\023\000\
\\024\000\019\000\000\000\
\\114\000\014\000\028\000\015\000\027\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\"
val actionRowNumbers =
"\012\000\066\000\064\000\061\000\
\\056\000\035\000\046\000\044\000\
\\042\000\034\000\033\000\010\000\
\\013\000\008\000\006\000\003\000\
\\002\000\011\000\023\000\050\000\
\\049\000\019\000\069\000\068\000\
\\004\000\018\000\018\000\060\000\
\\060\000\060\000\060\000\060\000\
\\060\000\060\000\059\000\059\000\
\\022\000\028\000\009\000\007\000\
\\020\000\000\000\005\000\026\000\
\\024\000\059\000\065\000\070\000\
\\063\000\062\000\058\000\057\000\
\\055\000\054\000\052\000\053\000\
\\051\000\021\000\045\000\043\000\
\\016\000\014\000\001\000\001\000\
\\037\000\002\000\036\000\067\000\
\\025\000\031\000\029\000\039\000\
\\038\000\048\000\047\000\017\000\
\\015\000\032\000\030\000\041\000\
\\040\000\027\000"
val gotoT =
"\
\\001\000\010\000\002\000\081\000\003\000\009\000\004\000\008\000\
\\005\000\007\000\006\000\006\000\007\000\005\000\008\000\004\000\
\\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\036\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\001\000\037\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\001\000\038\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\001\000\039\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\001\000\042\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\041\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\012\000\040\000\000\000\
\\001\000\042\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\041\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\012\000\043\000\000\000\
\\001\000\044\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\046\000\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\048\000\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\008\000\049\000\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\007\000\050\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\007\000\051\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\007\000\052\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\007\000\053\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\007\000\054\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\007\000\055\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\007\000\056\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\004\000\058\000\005\000\007\000\006\000\006\000\007\000\057\000\
\\008\000\004\000\009\000\003\000\010\000\002\000\011\000\001\000\000\000\
\\003\000\059\000\004\000\008\000\005\000\007\000\006\000\006\000\
\\007\000\057\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\068\000\004\000\008\000\005\000\007\000\006\000\006\000\
\\007\000\057\000\008\000\004\000\009\000\003\000\010\000\002\000\
\\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\069\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\001\000\070\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\001\000\071\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\001\000\072\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\001\000\042\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\041\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\012\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\077\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\001\000\078\000\003\000\009\000\004\000\008\000\005\000\007\000\
\\006\000\006\000\007\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\011\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 82
val numrules = 40
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | REAL of  (string)
 | INTEGER of  (string) | VARIABLE of  (string) | NAME of  (string)
 | TUPLE of  (Tuple) | FACTOR2 of  (Expr) | FACTOR1 of  (Expr)
 | FACTOR of  (Expr) | TERM of  (Expr) | AEXPR of  (Expr)
 | BFACTOR of  (Expr) | BBTERM of  (Expr) | BTERM of  (Expr)
 | BEXPR of  (Expr) | START of  (Expr) | EXPR of  (Expr)
end
type svalue = MlyValue.svalue
type result = Expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 27) => true | _ => false
val showTerminal =
fn (T 0) => "EMPTY"
  | (T 1) => "COMMA"
  | (T 2) => "IMPLIES"
  | (T 3) => "PERIOD"
  | (T 4) => "NAME"
  | (T 5) => "VARIABLE"
  | (T 6) => "INTEGER"
  | (T 7) => "REAL"
  | (T 8) => "NEG"
  | (T 9) => "TRUE"
  | (T 10) => "FALSE"
  | (T 11) => "PLUS"
  | (T 12) => "MINUS"
  | (T 13) => "MULT"
  | (T 14) => "DIV"
  | (T 15) => "GT"
  | (T 16) => "LT"
  | (T 17) => "GTE"
  | (T 18) => "LTE"
  | (T 19) => "NOT"
  | (T 20) => "AND"
  | (T 21) => "OR"
  | (T 22) => "EQUALS"
  | (T 23) => "LPAREN"
  | (T 24) => "RPAREN"
  | (T 25) => "LBRACKET"
  | (T 26) => "RBRACKET"
  | (T 27) => "EOF"
  | (T 28) => "PROJ"
  | (T 29) => "FUN"
  | (T 30) => "FUNAPPLY"
  | (T 31) => "IF"
  | (T 32) => "THEN"
  | (T 33) => "ELSE"
  | (T 34) => "FI"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXPR EXPR, EXPR1left, EXPR1right)) :: 
rest671)) => let val  result = MlyValue.START (EXPR)
 in ( LrTable.NT 1, ( result, EXPR1left, EXPR1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.BEXPR BEXPR, BEXPR1left, BEXPR1right)) :: 
rest671)) => let val  result = MlyValue.EXPR (expr(BEXPR))
 in ( LrTable.NT 0, ( result, BEXPR1left, BEXPR1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.AEXPR AEXPR, AEXPR1left, AEXPR1right)) :: 
rest671)) => let val  result = MlyValue.EXPR (expr(AEXPR))
 in ( LrTable.NT 0, ( result, AEXPR1left, AEXPR1right), rest671)
end
|  ( 3, ( ( _, ( _, _, RBRACKET1right)) :: ( _, ( MlyValue.TUPLE TUPLE
, _, _)) :: ( _, ( _, LBRACKET1left, _)) :: rest671)) => let val  
result = MlyValue.EXPR (tuple(TUPLE))
 in ( LrTable.NT 0, ( result, LBRACKET1left, RBRACKET1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.INTEGER INTEGER, _, INTEGER1right)) :: ( _, 
( MlyValue.TUPLE TUPLE, _, _)) :: ( _, ( _, PROJ1left, _)) :: rest671)
) => let val  result = MlyValue.EXPR (proj(TUPLE,INTEGER))
 in ( LrTable.NT 0, ( result, PROJ1left, INTEGER1right), rest671)
end
|  ( 5, ( ( _, ( _, _, EXPR2right)) :: _ :: ( _, ( MlyValue.EXPR EXPR,
 _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = 
MlyValue.EXPR (function(EXPR,EXPR))
 in ( LrTable.NT 0, ( result, FUN1left, EXPR2right), rest671)
end
|  ( 6, ( ( _, ( _, _, EXPR2right)) :: _ :: ( _, ( MlyValue.EXPR EXPR,
 _, _)) :: ( _, ( _, FUNAPPLY1left, _)) :: rest671)) => let val  
result = MlyValue.EXPR (funApply(EXPR,EXPR))
 in ( LrTable.NT 0, ( result, FUNAPPLY1left, EXPR2right), rest671)
end
|  ( 7, ( ( _, ( _, _, FI1right)) :: _ :: _ :: _ :: _ :: ( _, ( 
MlyValue.EXPR EXPR, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.EXPR (ifthenelse(EXPR,EXPR,EXPR))
 in ( LrTable.NT 0, ( result, IF1left, FI1right), rest671)
end
|  ( 8, ( ( _, ( _, _, END1right)) :: _ :: _ :: _ :: _ :: ( _, ( 
MlyValue.EXPR EXPR, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) =>
 let val  result = MlyValue.EXPR (letinend(EXPR,EXPR,EXPR))
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.EXPR (empty)
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.BTERM BTERM, BTERM1left, BTERM1right)) :: 
rest671)) => let val  result = MlyValue.BEXPR (expr(BTERM))
 in ( LrTable.NT 2, ( result, BTERM1left, BTERM1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.BEXPR BEXPR, _, BEXPR1right)) :: _ :: ( _, 
( MlyValue.BTERM BTERM, BTERM1left, _)) :: rest671)) => let val  
result = MlyValue.BEXPR (binOr(BTERM,BEXPR))
 in ( LrTable.NT 2, ( result, BTERM1left, BEXPR1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.BBTERM BBTERM, BBTERM1left, BBTERM1right))
 :: rest671)) => let val  result = MlyValue.BTERM (expr(BBTERM))
 in ( LrTable.NT 3, ( result, BBTERM1left, BBTERM1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.BTERM BTERM, _, BTERM1right)) :: _ :: ( _, 
( MlyValue.BBTERM BBTERM, BBTERM1left, _)) :: rest671)) => let val  
result = MlyValue.BTERM (binAnd(BBTERM,BTERM))
 in ( LrTable.NT 3, ( result, BBTERM1left, BTERM1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.BFACTOR BFACTOR, BFACTOR1left, 
BFACTOR1right)) :: rest671)) => let val  result = MlyValue.BBTERM (
expr(BFACTOR))
 in ( LrTable.NT 4, ( result, BFACTOR1left, BFACTOR1right), rest671)

end
|  ( 15, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.BEXPR BEXPR,
 _, _)) :: _ :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  
result = MlyValue.BBTERM (not(BEXPR))
 in ( LrTable.NT 4, ( result, NOT1left, RPAREN1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.TUPLE TUPLE, _, TUPLE1right)) :: _ :: ( _, 
( MlyValue.EXPR EXPR, EXPR1left, _)) :: rest671)) => let val  result =
 MlyValue.TUPLE (etuple(EXPR,TUPLE))
 in ( LrTable.NT 11, ( result, EXPR1left, TUPLE1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.AEXPR AEXPR, AEXPR1left, AEXPR1right)) :: 
rest671)) => let val  result = MlyValue.TUPLE (e(AEXPR))
 in ( LrTable.NT 11, ( result, AEXPR1left, AEXPR1right), rest671)
end
|  ( 18, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.BFACTOR (true)
 in ( LrTable.NT 5, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 19, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.BFACTOR (false)
 in ( LrTable.NT 5, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.VARIABLE VARIABLE, VARIABLE1left, 
VARIABLE1right)) :: rest671)) => let val  result = MlyValue.BFACTOR (
variable(VARIABLE))
 in ( LrTable.NT 5, ( result, VARIABLE1left, VARIABLE1right), rest671)

end
|  ( 21, ( ( _, ( _, _, AEXPR2right)) :: _ :: ( _, ( MlyValue.AEXPR 
AEXPR, AEXPR1left, _)) :: rest671)) => let val  result = 
MlyValue.BFACTOR (gt(AEXPR,AEXPR))
 in ( LrTable.NT 5, ( result, AEXPR1left, AEXPR2right), rest671)
end
|  ( 22, ( ( _, ( _, _, AEXPR2right)) :: _ :: ( _, ( MlyValue.AEXPR 
AEXPR, AEXPR1left, _)) :: rest671)) => let val  result = 
MlyValue.BFACTOR (gte(AEXPR,AEXPR))
 in ( LrTable.NT 5, ( result, AEXPR1left, AEXPR2right), rest671)
end
|  ( 23, ( ( _, ( _, _, AEXPR2right)) :: _ :: ( _, ( MlyValue.AEXPR 
AEXPR, AEXPR1left, _)) :: rest671)) => let val  result = 
MlyValue.BFACTOR (lt(AEXPR,AEXPR))
 in ( LrTable.NT 5, ( result, AEXPR1left, AEXPR2right), rest671)
end
|  ( 24, ( ( _, ( _, _, AEXPR2right)) :: _ :: ( _, ( MlyValue.AEXPR 
AEXPR, AEXPR1left, _)) :: rest671)) => let val  result = 
MlyValue.BFACTOR (lte(AEXPR,AEXPR))
 in ( LrTable.NT 5, ( result, AEXPR1left, AEXPR2right), rest671)
end
|  ( 25, ( ( _, ( _, _, AEXPR2right)) :: _ :: ( _, ( MlyValue.AEXPR 
AEXPR, AEXPR1left, _)) :: rest671)) => let val  result = 
MlyValue.BFACTOR (equals(AEXPR,AEXPR))
 in ( LrTable.NT 5, ( result, AEXPR1left, AEXPR2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.TERM TERM, TERM1left, TERM1right)) :: 
rest671)) => let val  result = MlyValue.AEXPR (expr(TERM))
 in ( LrTable.NT 6, ( result, TERM1left, TERM1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.AEXPR AEXPR, _, AEXPR1right)) :: _ :: ( _, 
( MlyValue.TERM TERM, TERM1left, _)) :: rest671)) => let val  result =
 MlyValue.AEXPR (add(TERM,AEXPR))
 in ( LrTable.NT 6, ( result, TERM1left, AEXPR1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.AEXPR AEXPR, _, AEXPR1right)) :: _ :: ( _, 
( MlyValue.TERM TERM, TERM1left, _)) :: rest671)) => let val  result =
 MlyValue.AEXPR (sub(TERM,AEXPR))
 in ( LrTable.NT 6, ( result, TERM1left, AEXPR1right), rest671)
end
|  ( 29, ( rest671)) => let val  result = MlyValue.AEXPR (empty)
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 30, ( ( _, ( MlyValue.FACTOR FACTOR, FACTOR1left, FACTOR1right))
 :: rest671)) => let val  result = MlyValue.TERM (expr(FACTOR))
 in ( LrTable.NT 7, ( result, FACTOR1left, FACTOR1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.TERM TERM, _, TERM1right)) :: _ :: ( _, ( 
MlyValue.FACTOR FACTOR, FACTOR1left, _)) :: rest671)) => let val  
result = MlyValue.TERM (mul(FACTOR,TERM))
 in ( LrTable.NT 7, ( result, FACTOR1left, TERM1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.TERM TERM, _, TERM1right)) :: _ :: ( _, ( 
MlyValue.FACTOR FACTOR, FACTOR1left, _)) :: rest671)) => let val  
result = MlyValue.TERM (divide(FACTOR,TERM))
 in ( LrTable.NT 7, ( result, FACTOR1left, TERM1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.FACTOR1 FACTOR1, FACTOR11left, 
FACTOR11right)) :: rest671)) => let val  result = MlyValue.FACTOR (
expr(FACTOR1))
 in ( LrTable.NT 8, ( result, FACTOR11left, FACTOR11right), rest671)

end
|  ( 34, ( ( _, ( MlyValue.FACTOR1 FACTOR1, _, FACTOR11right)) :: ( _,
 ( _, NEG1left, _)) :: rest671)) => let val  result = MlyValue.FACTOR
 (neg(FACTOR1))
 in ( LrTable.NT 8, ( result, NEG1left, FACTOR11right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.FACTOR2 FACTOR2, FACTOR21left, 
FACTOR21right)) :: rest671)) => let val  result = MlyValue.FACTOR1 (
expr(FACTOR2))
 in ( LrTable.NT 9, ( result, FACTOR21left, FACTOR21right), rest671)

end
|  ( 36, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXPR EXPR, _
, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.FACTOR1 (expr(EXPR))
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.INTEGER INTEGER, INTEGER1left, 
INTEGER1right)) :: rest671)) => let val  result = MlyValue.FACTOR2 (
n(INTEGER))
 in ( LrTable.NT 10, ( result, INTEGER1left, INTEGER1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.REAL REAL, REAL1left, REAL1right)) :: 
rest671)) => let val  result = MlyValue.FACTOR2 (r(REAL))
 in ( LrTable.NT 10, ( result, REAL1left, REAL1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.VARIABLE VARIABLE, VARIABLE1left, 
VARIABLE1right)) :: rest671)) => let val  result = MlyValue.FACTOR2 (
variable(VARIABLE))
 in ( LrTable.NT 10, ( result, VARIABLE1left, VARIABLE1right), rest671
)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Logic_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EMPTY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun PERIOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.NAME i,p1,p2))
fun VARIABLE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VARIABLE i,p1,p2))
fun INTEGER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.INTEGER i,p1,p2))
fun REAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.REAL i,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun MULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun GTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACKET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun PROJ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNAPPLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
end
end
