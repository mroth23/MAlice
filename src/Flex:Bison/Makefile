Parser: main.cpp lexer.cpp parser.cpp
	g++ -Wall -pedantic -o Parser main.cpp lexer.cpp parser.cpp ast2.h

parser.cpp: parser.y
	bison -d -o parser.cpp parser.y

lexer.cpp: lexer.l
	lex -o lexer.cpp lexer.l

.PHONY: clean
clean:
	-rm -f *.o	
	-rm -f Parser
	-rm -f parser.hpp
	-rm -f lexer.cpp
	-rm -f parser.cpp
