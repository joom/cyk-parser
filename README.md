# cyk-parser

A CYK parser for context-free grammars, implemented in Haskell.

It reads a grammar from a file, and then sentences to parse according to that grammar from another file, and prints the output as JSON. For partial parses, it prints the longest partial parse. The parsed form is printed as an S-expression.

## Usage

In order to use or compile the program you need to have [Stack](http://haskellstack.org) installed.

After you cloning the repository, go to the repository folder and do

```bash
stack install
```

Now you installed the program. You can run it like this:

```
cyk-parser-exe -g grammar.txt -s sentences.txt
```

which will print:

```json
[
    {
        "fullParse": true,
        "original": "light cars fly",
        "tree": [
            "(S (NP (N light) (NBar cars)) (VP fly))"
        ]
    },
    {
        "fullParse": false,
        "original": "light green cars fly",
        "tree": [
            "(NP' (Adj green) (NBar cars))"
        ]
    },
    {
        "fullParse": true,
        "original": "the man gave a baby a car",
        "tree": [
            "(S (NP (Det the) (NBar man)) (VP (V gave) (VP' (NP (Det a) (NBar baby)) (NP (Det a) (NBar car)))))"
        ]
    },
    {
        "fullParse": true,
        "original": "the baby put the man on the truck",
        "tree": [
            "(S (NP (Det the) (NBar baby)) (VP (VP (V put) (NP (Det the) (NBar man))) (PP (Prep on) (NP (Det the) (NBar truck)))))",
            "(S (NP (Det the) (NBar baby)) (VP (V put) (NP (NP (Det the) (NBar man)) (PP (Prep on) (NP (Det the) (NBar truck))))))",
            "(S (NP (Det the) (NBar baby)) (VP (V put) (VP' (NP (Det the) (NBar man)) (PP (Prep on) (NP (Det the) (NBar truck))))))"
        ]
    }
]
```
