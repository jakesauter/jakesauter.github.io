% s([cheetas, are, the, fastest, animals, on, earth],[]).

s(X,Z):-  np(X,Y),  vp(Y,Z). 
    
pp(X,Z):- p(X,Y), n(Y,Z) ; p(X,Y), np(Y,Z) ; pp(X,Y), pp(Y,Z).

np(X,Z):- p(X,Y), n(Y,Z) ; det(X,Y), np(Y,Z) ; det(X,Y), pp(Y,Z) ;  n(X,Z).

vp(X,Z):-  v(X,Y),  np(Y,Z).
    
det([the|W],W). 
    
n([earth|W],W). 
n([animals|W],W).
n([cheetas|W],W).
 
p([fastest|W],W).
p([on|W],W).

v([are|W],W).

