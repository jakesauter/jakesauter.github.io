% s([black, mambas, are, the, most, dangerous, snakes, in, the, world],[]).

s  -->  np,vp. 
vp  -->  v, np, pp. 
pp --> p, np.
v --> [are].
det  -->  [the].
np  -->  det,n ; p,n ; det, pp.
n  -->  [mambas] ; [world] ; [snakes]. 
p --> [black] ; [in] ; [most] ; [dangerous].
