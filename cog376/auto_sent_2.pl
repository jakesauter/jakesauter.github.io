% s([cheetas, are, the, fastest, animals, on, earth],[]).

s  -->  np,vp. 
vp  -->  v, np, np.
pp --> p, np.
v --> [are].
det  -->  [the].
np  -->  [cheetas] ; p,n ; det, np.
n  -->  [earth] ; [animals]. 
p --> [fastest] ; [on].
