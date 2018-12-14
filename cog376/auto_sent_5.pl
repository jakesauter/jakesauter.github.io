% s([the, african, wild, dog, is, a, very, vocal, animal],[]).
  
s  -->  np,vp. 
vp  -->  v, np. 
pp --> p, np.
v --> [is].
det  -->  [the] ; [a].
np  -->  det,n ; p,n ; n, pp ; det, pp.
n  -->  [dog] ; [animal]. 
p --> [african] ; [wild] ; [vocal] ; [very].
