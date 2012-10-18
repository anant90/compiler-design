male(shantanu).
male(bhishma).
male(vichitravirya).
male(dhritarashtra).
male(duryodhan).
male(pandu).
male(yudhishtra).
male(bhima).
male(arjun).
male(nakul).
male(sahadev).
male(karna).

female(ganga).
female(satyavati).
female(ambika).
female(gandhari).
female(kunti).
female(madri).

married(shantanu, ganga).
married(shantanu,satyavati).
married(vichitravirya,ambika).
married(dhritarashtra,gandhari).
married(pandu,kunti).
married(pandu,madri).

child(bhishma, shantanu).
child(bhishma, ganga).
child(vichitravirya,shantanu).
child(vichitravirya,satyavati).
child(dhritarashtra,ambika).
child(dhritarashtra,vichitravirya).
child(duryodhan,gandhari).
child(duryodhan,dhritarashtra).
child(yudhishtra,pandu).
child(yudhishtra,kunti).
child(bhima,pandu).
child(bhima,kunti).
child(arjun,pandu).
child(arjun,kunti).
child(nakul,pandu).
child(nakul,madri).
child(sahadev,pandu).
child(sahadev,madri).
child(karna,kunti).

son(S,P):-child(S,P),male(S).
daughter(D,P):-child(D,P),female(D).
father(F,C):-child(C,F),male(F).
mother(M,C):-child(C,M),female(M).
parent(P,C):-child(C,P).
parents(F,M,C):-father(F,C),mother(M,C).
brothers(X,Y):-male(Y),brother(X,Y).
sisters(X,Y):-female(Y),sister(X,Y).
husband(H,W):-male(H),female(W),married(H,W).
wife(W,H):-husband(H,W).
anc(X,Y):-parent(X,Y).
anc(X,Y):-parent(X,Z), anc(Z,Y).
cousin(X,Y):-parent(Z,X),parent(W,Y),sibling(Z,W).
grandparent(GP,GC):-parent(P,GC),parent(GP,P).
granmother(GM,GC):-grandparent(GM,GC),female(GM).
grandfather(GF,GC):-grandparent(GF,GC),male(GF).
