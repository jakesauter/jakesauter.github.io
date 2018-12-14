sent = "Cheetas are the fastest animals on Earth".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP NP 
  PP -> P NP
  V ->  "are"  
  Det -> "the" 
  NP ->  "Cheetas" | P N | Det NP
  N ->  "Earth" | "animals"
  P -> "fastest" | "on"
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()

