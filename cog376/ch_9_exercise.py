# The example
kim = {'CAT': 'NP', 'ORTH': 'Kim', 'REF': 'k'}
chase = {'CAT': 'V', 'ORTH': 'chased', 'REL': 'chase'}
chase['AGT'] = 'sbj'
chase['PAT'] = 'obj'
sent = "Kim chased Lee"
tokens = sent.split()
lee = {'CAT': 'NP', 'ORTH': 'Lee', 'REF': 'l'}
def lex2fs(word):
    for fs in [kim, lee, chase]:
        if fs['ORTH'] == word:
            return fs
subj, verb, obj = lex2fs(tokens[0]), lex2fs(tokens[1]), lex2fs(tokens[2])
verb['AGT'] = subj['REF']
verb['PAT'] = obj['REF']
for k in ['ORTH', 'REL', 'AGT', 'PAT']:
    print("%-5s => %s" % (k, verb[k]))

# My first try
billy = {'CAT': 'NP', 'ORTH': 'Billy', 'REF': 'b'}
likes = {'CAT': 'V', 'ORTH': 'likes', 'REL': 'like'}
sally = {'CAT': 'NP', 'ORTH': 'Sally', 'REF': 's'}
likes['AGT'] = 'sbj'
likes['PAT'] = 'obj'
sent = "Billy likes Sally"
tokens = sent.split()
def lex2fs(word):
    for fs in [billy, sally, likes]:
        if fs['ORTH'] == word:
            return fs
subj, verb, obj = lex2fs(tokens[0]), lex2fs(tokens[1]), lex2fs(tokens[2])
verb['AGT'] = subj['REF']
verb['PAT'] = obj['REF']
for k in ['ORTH', 'REL', 'AGT', 'PAT']:
    print("%-5s => %s" % (k, verb[k]))

# My second try 
billy = {'CAT': 'NP', 'ORTH': 'Billy', 'REF': 'b'}
was = {'CAT': 'V', 'ORTH': 'was', 'REL': 'w'}
nervous = {'CAT': 'P', 'ORTH': 'nervous', 'REF': 'n'}
was['AGT'] = 'sbj'
was['PAT'] = 'obj'
sent = "Billy was nervous"
tokens = sent.split()
def lex2fs(word):
    for fs in [billy, was, nervous]:
        if fs['ORTH'] == word:
            return fs
subj, verb, obj = lex2fs(tokens[0]), lex2fs(tokens[1]), lex2fs(tokens[2])
verb['AGT'] = subj['REF']
verb['PAT'] = obj['REF']
for k in ['ORTH', 'REL', 'AGT', 'PAT']:
    print("%-5s => %s" % (k, verb[k]))
