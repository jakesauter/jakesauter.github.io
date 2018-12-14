sent = "The Giraffe is the tallest animal in the world".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP  
  PP -> P NP
  V -> "is"  
  NP ->  Det N | P N | Det NP | Det PP | N PP
  Det -> "The" | "the"
  N ->  "Giraffe" | "world" | "animal"
  P -> "tallest" | "in"
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()

