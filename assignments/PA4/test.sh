#!/bin/bash

lexer $1 | parser | semant > out1.txt 2>&1
lexer $1 | parser | ./semant > out2.txt 2>&1
diff out1.txt out2.txt > diff.txt
