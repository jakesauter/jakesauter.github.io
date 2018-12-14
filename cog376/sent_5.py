sent = "Hyenas are generally nocturnal".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP  
  PP -> P NP 
  V ->  "are"  
  NP ->  "Hyenas" | P N 
  N ->  "nocturnal" 
  P -> "generally" 
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()

