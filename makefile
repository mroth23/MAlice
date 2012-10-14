CXX=clang++
CFLAGS=-O2 -Wall -Werror -Wextra -pedantic
LDFLAGS=-O2 -Wall -Werror -Wextra -pedantic

OUT=bin/malice
IDIR=inc
ODIR=obj
SDIR=src

_OBJS = main.o
OBJS = $(patsubst %,$(ODIR)/%,$(_OBJS))

all: $(OUT)

debug: CXX += -DDEBUG -g
debug: $(OUT)

$(ODIR)/%.o: $(SDIR)/%.cpp 
	$(CXX) -c $(INC) -o $@ $< $(CFLAGS) 

$(OUT): $(OBJS)
	$(CXX) -o $@ $^ $(LDFLAGS) $(LIBS)
	strip $(OUT)

clean:
	rm -f $(ODIR)/*.o $(OUT)