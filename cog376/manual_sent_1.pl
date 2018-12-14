% s([the, spotted, hyena, is, the, largest, of, the, species], []).


s(X,Z):-  np(X,Y),  vp(Y,Z). 
np(X,Z):-  det(X,Y),  n(Y,Z) ; p(X,Y), n(Y,Z) ; det(X,Y), np(Y,Z) ; det(X,Y), pp(Y,Z) .
vp(X,Z):-  v(X,Y),  np(Y,Z). 
pp(X,Z):- p(X,Y), np(Y,Z) ; p(X,Y), pp(Y,Z).
det([the|W],W).     
n([hyena|W],W). 
n([species|W],W).
p([largest|W],W).
p([of|W],W).
p([spotted|W],W).
v([is|W],W).

