fun reverse [] = []
  | reverse (a::tl) = reverse(tl)@[a]

fun stringtoint(a) = 
    let fun stringint(#"0"::bs) = 10*stringint(bs)
        | stringint(#"1"::bs) = 1+10*stringint(bs)
        | stringint(#"2"::bs) = 2+10*stringint(bs)
        | stringint(#"3"::bs) = 3+10*stringint(bs)
        | stringint(#"4"::bs) = 4+10*stringint(bs)
        | stringint(#"5"::bs) = 5+10*stringint(bs)
        | stringint(#"6"::bs) = 6+10*stringint(bs)
        | stringint(#"7"::bs) = 7+10*stringint(bs)
        | stringint(#"8"::bs) = 8+10*stringint(bs)
        | stringint(#"9"::bs) = 9+10*stringint(bs)
        | stringint([]) = 0
        | stringint(hd::bs) = stringint(bs)
    in stringint(reverse(explode(a)))
    end
    
fun power(a,0) = 1.0
    | power(a:real,n:int) = a*power(a,n-1);
    
fun decimal((#".")::bs) = 0
    | decimal(hd::bs) = 1+decimal(bs)
    | decimal([])=0;
    
fun stringtoreal(a) = 
    let fun stringreal(#"0"::bs) = 10.0*stringreal(bs)
        | stringreal(#"1"::bs) = 1.0+10.0*stringreal(bs)
        | stringreal(#"2"::bs) = 2.0+10.0*stringreal(bs)
        | stringreal(#"3"::bs) = 3.0+10.0*stringreal(bs)
        | stringreal(#"4"::bs) = 4.0+10.0*stringreal(bs)
        | stringreal(#"5"::bs) = 5.0+10.0*stringreal(bs)
        | stringreal(#"6"::bs) = 6.0+10.0*stringreal(bs)
        | stringreal(#"7"::bs) = 7.0+10.0*stringreal(bs)
        | stringreal(#"8"::bs) = 8.0+10.0*stringreal(bs)
        | stringreal(#"9"::bs) = 9.0+10.0*stringreal(bs)
        | stringreal([])=0.0
        | stringreal(#"."::bs) = stringreal(bs)
        | stringreal(hd::bs) = stringreal(bs)
    in ((power(0.1,decimal(reverse(explode(a)))))*stringreal(reverse(explode(a))))
    end
