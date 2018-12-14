sent = "The African wild dog is a very vocal animal".split()
grammar = CFG.fromstring ("""
  S -> NP VP
  VP -> V NP 
  PP -> P NP  
  V ->  "is"  
  Det -> "The" | "a"
  NP ->  Det N | P N | N PP | Det PP 
  N ->  "dog" | "animal"
  P -> "African" | "wild" | "vocal" | "very"
  """)
parser = nltk.RecursiveDescentParser(grammar)
trees = parser.parse(sent)
for tree in trees:
  tree.draw()

