sent = "Giraffes can live 25 years in the wild".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP  
  PP -> P NP 
  V ->  "can" "live" 
  Det -> "the"
  NP ->  "Giraffes" | Det N | P N | N PP | P NP
  N ->  "years" | "wild"
  P -> "25" | "in" 
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()


