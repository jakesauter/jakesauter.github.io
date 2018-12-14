% s([leopards, are, formidable, predators],[]).


s(X,Z):-  np(X,Y),  vp(Y,Z). 
    
pp(X,Z):- p(X,Y), np(Y,Z).

np(X,Z):- p(X,Y), n(Y,Z) ; n(X,Z).
  
vp(X,Z):-  v(X,Y),  np(Y,Z). 
    
det([the|W],W). 
    
n([leopards|W],W). 
n([predators|W],W).
 
p([formidable|W],W).

v([are|W],W).

