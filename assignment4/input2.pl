edge(a,b).
edge(a,e).
edge(b,d).
edge(c,a).
edge(e,b).
edge(f,c).
path(Node1,Node2) :- edge(Node1,Node2).
path(Node1,Node2) :- edge(Node1,SomeNode), path(SomeNode,Node2).
