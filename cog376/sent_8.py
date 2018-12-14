sent = "Black Mambas are the most dangerous snakes in the world".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP PP 
  PP -> P NP 
  V ->  "are"  
  Det -> "the"
  NP ->  "Giraffes" | Det N | P N | Det PP 
  N ->  "Mambas" | "world" | "snakes"
  P -> "Black" | "in" | "most" | "dangerous"
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()

