sent = "Elephants are the largest land mammal".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP  
  PP -> P NP
  V -> "are"  
  NP -> "Elephants" | Det N | P N | Det NP | Det PP 
  Det -> "the"  
  N ->  "mammal" 
  P -> "largest" | "land"
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()
               
