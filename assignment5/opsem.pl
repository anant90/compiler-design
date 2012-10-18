append([],L,L).
append([X|L],M,[X|N]) :- append(L,M,N).

search([],A,empty).
search([(A,C)|B],A,C).
search([(C,D)|B],A,E) :- search(B,A,E).

hastype1(Tau,bind(A,B),[(A,D)]) :- hastype(Tau,B,D).
hastype1(Tau,compose(A,B),E) :- hastype1(Tau,A,S),append(S,Tau,R),hastype1(R,B,Q),append(Q,S,E).
hastype1(Tau,parallel(A,B),E) :- hastype1(Tau,A,S),hastype1(Tau,B,R),append(S,R,E).
hastype1(Tau,local(A,B),E) :- hastype1(Tau,A,S),append(S,Tau,R),hastype1(R,B,E).

hastype(Tau,empty,'').
hastype(Tau,var(A),C) :- search(Tau,A,C).
hastype(Tau,integer(X),'int').
hastype(Tau,real(X),'real').
hastype(Tau,true,'bool').
hastype(Tau,false,'bool').
hastype(Tau,empty,'unit').
hastype(Tau,neg(X),'int'):-hastype(Tau,X,'int').
hastype(Tau,neg(X),'real'):-hastype(Tau,X,'real').
hastype(Tau,plus(X,Y),'int'):-hastype(Tau,X,'int'),hastype(Tau,Y,'int').
hastype(Tau,plus(X,Y),'real'):-hastype(Tau,X,'real'),hastype(Tau,Y,'real').
hastype(Tau,minus(X,Y),'int'):-hastype(Tau,X,'int'),hastype(Tau,Y,'int').
hastype(Tau,minus(X,Y),'real'):-hastype(Tau,X,'real'),hastype(Tau,Y,'real').
hastype(Tau,multiply(X,Y),'int'):-hastype(Tau,X,'int'),hastype(Tau,Y,'int').
hastype(Tau,multiply(X,Y),'real'):-hastype(Tau,X,'real'),hastype(Tau,Y,'real').
hastype(Tau,divide(X,Y),'real'):-hastype(Tau,X,'real'),hastype(Tau,Y,'real').
hastype(Tau,greatereq(X,Y),'bool'):-hastype(Tau,X,'real'),hastype(Tau,Y,'real').
hastype(Tau,greatereq(X,Y),'bool'):-hastype(Tau,X,'int'),hastype(Tau,Y,'int').
hastype(Tau,lesseq(X,Y),'bool'):-hastype(Tau,X,'real'),hastype(Tau,Y,'real').
hastype(Tau,lesseq(X,Y),'bool'):-hastype(Tau,X,'int'),hastype(Tau,Y,'int').
hastype(Tau,greater(X,Y),'bool'):-hastype(Tau,X,'real'),hastype(Tau,Y,'real').
hastype(Tau,greater(X,Y),'bool'):-hastype(Tau,X,'int'),hastype(Tau,Y,'int').
hastype(Tau,less(X,Y),'bool'):-hastype(Tau,X,'real'),hastype(Tau,Y,'real').
hastype(Tau,less(X,Y),'bool'):-hastype(Tau,X,'int'),hastype(Tau,Y,'int').
hastype(Tau,equal(X,Y),'bool'):-hastype(Tau,X,'int'),hastype(Tau,Y,'int').
hastype(Tau,andd(X,Y),'bool'):-hastype(Tau,X,'bool'),hastype(Tau,Y,'bool').
hastype(Tau,orr(X,Y),'bool'):-hastype(Tau,X,'bool'),hastype(Tau,Y,'bool').
hastype(Tau,nott(X),'bool'):-hastype(Tau,X,'bool').
hastype(Tau,tuple([]),[]).
hastype(Tau,tuple([A|B]),R):-hastype(Tau,A,X), hastype(Tau,tuple(B),Y), append([X],Y,R).
hastype(Tau,let(A,B),C) :-  hastype1(Tau,A,D),append(D,Tau,E),hastype(E,B,C).
hastype(Tau,(lamda(A,B),C),D) :- hastype(Tau,let(bind(A,C),B),D).

appendbig([],L,L).
appendbig([X|L],M,[X|N]) :- appendbig(L,M,N).

% Tau=[(A,B)]	%A->variable		B->type in string

bigstep1(Gamma,bind(A,B),[(A,D)]) :- bigstep(Gamma,B,D).
bigstep1(Gamma,compose(A,B),E) :- bigstep1(Gamma,A,S),append(S,Gamma,R),bigstep1(R,B,Q),append(Q,S,E).
bigstep1(Gamma,parallel(A,B),E) :- bigstep1(Gamma,A,S),bigstep1(Gamma,B,R),append(S,R,E).
bigstep1(Gamma,local(A,B),E) :- bigstep1(Gamma,A,S),append(S,Gamma,R),bigstep1(R,B,E).

bigstep(Gamma,empty,'').
bigstep(Gamma,var(A),B) :- search(Gamma,A,C),bigstep(Gamma,C,B).
bigstep(Gamma,integer(X),integer(X)).
bigstep(Gamma,real(X),real(X)).
bigstep(Gamma,true,true).
bigstep(Gamma,false,false).
bigstep(Gamme,neg(X),integer(Z)):-bigstep(Gamma,X,integer(A)), Z is -A.
bigstep(Gamme,neg(X),real(Z)):-bigstep(Gamma,X,real(A)), Z is -A.
bigstep(Gamma,plus(X,Y),integer(Z)):- bigstep(Gamma,X,integer(A)),bigstep(Gamma,Y,integer(B)),Z is A + B.
bigstep(Gamma,plus(X,Y),real(Z)):- bigstep(Gamma,X,real(A)),bigstep(Gamma,Y,real(B)),Z is A + B.
bigstep(Gamma,minus(X,Y),integer(Z)):- bigstep(Gamma,X,integer(A)),bigstep(Gamma,Y,integer(B)),Z is A - B.
bigstep(Gamma,minus(X,Y),real(Z)):- bigstep(Gamma,X,real(A)),bigstep(Gamma,Y,real(B)),Z is A - B.
bigstep(Gamma,multiply(X,Y),integer(Z)):- bigstep(Gamma,X,integer(A)),bigstep(Gamma,Y,integer(B)),Z is A * B.
bigstep(Gamma,multiply(X,Y),real(Z)):- bigstep(Gamma,X,real(A)),bigstep(Gamma,Y,real(B)),Z is A * B.
bigstep(Gamma,divide(X,Y),real(Z)):- bigstep(Gamma,X,real(A)),bigstep(Gamma,Y,real(B)),Z is A / B.
bigstep(Gamma,greatereq(X,Y),true) :- bigstep(Gamma,X,integer(A)), bigstep(Gamma,Y,integer(B)), A>=B.
bigstep(Gamma,greatereq(X,Y),false) :- bigstep(Gamma,X,integer(A)), bigstep(Gamma,Y,integer(B)), A<B.
bigstep(Gamma,greatereq(X,Y),true) :- bigstep(Gamma,X,real(A)), bigstep(Gamma,Y,real(B)), A>=B.
bigstep(Gamma,greatereq(X,Y),false) :- bigstep(Gamma,X,real(A)), bigstep(Gamma,Y,real(B)), A<B.
bigstep(Gamma,greater(X,Y),true) :- bigstep(Gamma,X,integer(A)), bigstep(Gamma,Y,integer(B)), A>B.
bigstep(Gamma,greater(X,Y),true) :- bigstep(Gamma,X,real(A)), bigstep(Gamma,Y,real(B)), A>B.
bigstep(Gamma,greater(X,Y),false) :- bigstep(Gamma,X,integer(A)), bigstep(Gamma,Y,integer(B)), A=<B.
bigstep(Gamma,greater(X,Y),false) :- bigstep(Gamma,X,real(A)), bigstep(Gamma,Y,real(B)), A=<B.
bigstep(Gamma,lesseq(X,Y),true) :- bigstep(Gamma,X,integer(A)), bigstep(Gamma,Y,integer(B)), A=<B.
bigstep(Gamma,lesseq(X,Y),true) :- bigstep(Gamma,X,real(A)), bigstep(Gamma,Y,real(B)), A=<B.
bigstep(Gamma,lesseq(X,Y),false) :- bigstep(Gamma,X,integer(A)), bigstep(Gamma,Y,integer(B)), A>B.
bigstep(Gamma,lesseq(X,Y),false) :- bigstep(Gamma,X,real(A)), bigstep(Gamma,Y,real(B)), A>B.
bigstep(Gamma,less(X,Y),true) :- bigstep(Gamma,X,integer(A)), bigstep(Gamma,Y,integer(B)), A<B.
bigstep(Gamma,less(X,Y),false) :- bigstep(Gamma,X,integer(A)), bigstep(Gamma,Y,integer(B)), A>=B.
bigstep(Gamma,less(X,Y),true) :- bigstep(Gamma,X,real(A)), bigstep(Gamma,Y,real(B)), A<B.
bigstep(Gamma,less(X,Y),false) :- bigstep(Gamma,X,real(A)), bigstep(Gamma,Y,real(B)), A>=B.
bigstep(Gamma,equal(X,Y),true) :- bigstep(Gamma,X,Z),bigstep(Gamma,Y,Z).
bigstep(Gamma,equal(X,Y),false) :- bigstep(Gamma,X,Z), bigstep(Gamma,Y,A), Z\=A.
bigstep(Gamma,andd(X,Y),true) :- bigstep(Gamma,X,true),bigstep(Gamma,Y,true).
bigstep(Gamma,andd(X,Y),false) :- bigstep(Gamma,X,true),bigstep(Gamma,Y,false).
bigstep(Gamma,andd(X,Y),false) :- bigstep(Gamma,X,false),bigstep(Gamma,Y,false).
bigstep(Gamma,andd(X,Y),false) :- bigstep(Gamma,X,false),bigstep(Gamma,Y,true).
bigstep(Gamma,orr(X,Y),true) :- bigstep(Gamma,X,true),bigstep(Gamma,Y,true).
bigstep(Gamma,orr(X,Y),true) :- bigstep(Gamma,X,true),bigstep(Gamma,Y,false).
bigstep(Gamma,orr(X,Y),false) :- bigstep(Gamma,X,false),bigstep(Gamma,Y,false).
bigstep(Gamma,orr(X,Y),true) :- bigstep(Gamma,X,false),bigstep(Gamma,Y,true).
bigstep(Gamma,nott(X),true) :- bigstep(Gamma,X,false).
bigstep(Gamma,nott(X),false) :- bigstep(Gamma,X,true).
bigstep(Gamma,tuple([]),[]).
bigstep(Gamma,tuple([A|B]),R) :- bigstep(Gamma,A,X), bigstep(Gamma,tuple(B),Y), appendbig([X],Y,R).
bigstep(Gamma,let(A,B),C) :-  bigstep1(Gamma,A,D),append(D,Gamma,E),bigstep(E,B,C).
bigstep(Gamma,(lamda(A,B),C),D) :- bigstep(Gamma,let(bind(A,C),B),D).

%bigstep([(x,integer(5))],let(bind(x,plus(integer(1),integer(2))),plus(var(x),integer(4))),X).
%bigstep([(x,integer(5))],(lamda(y,plus(var(y),integer(3))),minus(integer(3),integer(1))),X).
