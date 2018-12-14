% s([the, african, wild, dog, is, a, very, vocal, animal],[]).

s(X,Z):-  np(X,Y),  vp(Y,Z). 
    
pp(X,Z):- p(X,Y), np(Y,Z).

np(X,Z):-  det(X,Y),  n(Y,Z) ; p(X,Y), n(Y,Z) ; n(X,Y), pp(Y,Z) ; det(X,Y), pp(Y,Z) .
  
vp(X,Z):-  v(X,Y),  np(Y,Z). 
    
det([the|W],W). 
det([a|W],W). 
    
n([dog|W],W). 
n([animal|W],W).
 
p([african|W],W).
p([wild|W],W).
p([vocal|W],W).
p([very|W],W).

v([is|W],W).

