sent = "Leopards are formidable predators".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP 
  PP -> P NP 
  V ->  "are"
  NP ->  "Leopards" | P N
  N ->  "predators"
  P -> "formidable"
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()

