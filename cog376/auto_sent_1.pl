% s([the, spotted, hyena, is, the, largest, of, the, species], []).

s  -->  np,vp. 
np  -->  det,n ; p,n ; det, np ; det, pp.
vp  -->  v, np. 
pp --> p, np ; p, pp.
v --> [is].
det  -->  [the].
n  -->  [hyena] ; [species]. 
p --> [largest] ; [of] ; [spotted].
