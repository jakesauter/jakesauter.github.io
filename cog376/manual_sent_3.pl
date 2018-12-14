% s([black, mambas, are, the, most, dangerous, snakes, in, the, world],[]).

s(X,Z):-  np(X,Y),  vp(Y,Z). 
    
vp(X,Z):-  v(X,Y),  np(Y,Z). 

pp(X,Z):- p(X,Y), np(Y,Z).

np(X,Z):-  det(X,Y),  n(Y,Z) ; p(X,Y), n(Y,Z) ; det(X,Y),  pp(Y,Z) ; np(X,Y), pp(Y,Z).
 
    
det([the|W],W). 
    
n([mambas|W],W). 
n([world|W],W).
n([snakes|W],W).

p([black|W],W).
p([in|W],W).
p([most|W],W).
p([dangerous|W],W).

v([are|W],W).

