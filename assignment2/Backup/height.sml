
fun htp(Absyn.EMPTY)=0
	|htp(Absyn.PR(a,b))=1+max(hts(a),htp(b))
and hts(Absyn.FT(f))=1+htf(f)
	|hts(Absyn.RL(r))=1+htr(r)
and htf(Absyn.LT(l))=1+htl(l)
and htr(Absyn.HB(h,b))=1+max(hth(h),htb(b))
and hth(Absyn.HD(h))=1+htl(h)
and htb(Absyn.BD(l))=1+htl(l)
	|htb(Absyn.LB(l,b))=1+max(htl(l),htb(b))
and htl(Absyn.LNM(n))=1
	|htl(Absyn.LNT(n,t))=1+htt(t)
and htt(Absyn.TU(t))=1+htlt(t)
and htlt(Absyn.L1(t))=1+httm(t)
	|htlt(Absyn.L2(t,lt))=1+max(httm(t),htlt(lt))
and httm(Absyn.T1(n))=1
	|httm(Absyn.T2(v))=1
	|httm(Absyn.T3(n,t))=1+htt(t)
and max(a,b)  = if a>b then a else b;
