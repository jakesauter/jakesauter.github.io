sent = "The spotted Hyena is the largest of the species".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP  
  PP -> P NP | P PP
  V ->  "is"  
  Det -> "The" | "the"
  NP ->  Det N | P N | Det NP | Det PP
  N ->  "Hyena" | "species"
  P -> "largest" | "of" | "spotted"
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()

