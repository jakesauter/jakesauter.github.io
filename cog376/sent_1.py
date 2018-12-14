sent = "The lifespan of Baboons is 25 years".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP  
  PP -> P NP
  V -> "is"  
  NP -> Det N | P N | Det NP | N NP
  Det -> "The"  
  N -> "lifespan" | "years" | "Baboons"
  P -> "of" | "25"
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()
