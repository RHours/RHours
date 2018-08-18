fslexyacc\fsyacc.exe JsonGrammar.fsy -o JsonParser.fs --module JsonParser --internal

fslexyacc\fslex.exe JsonLexer.fsl -o JsonLexer.fs --unicode

