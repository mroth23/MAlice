CXX=clang++
#CXXFLAGS=-O0 -Wall -Werror -Wextra -pedantic -g -DDEBUG
CXXFLAGS=-O3 -Wall -Werror -Wextra -pedantic -DDEBUG
#LDFLAGS=-g -Wall -Werror -Wextra -pedantic
LDFLAGS=-O3 -Wall -Werror -Wextra -pedantic

.PHONY: default clean again

default: malice

main.o: main.cpp

malice: main.o
	$(CXX) $(LDFLAGS) -o $@ $^

clean:
	rm -f main.o
	rm -f malice

again: clean default