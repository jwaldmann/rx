
MAIN = RX
CGI = $(MAIN)_CGI

LIBRX = librx.a

GHC = ghc -fglasgow-exts
OPT = -O2

SRC = $(wildcard *.hs)

# compilation stuff ######################################################

all : $(MAIN) # $(CGI)

$(MAIN) : $(SRC)
	$(GHC) $(OPT) --make -o $@ $@.hs

$(CGI) : $(SRC)
	$(GHC) $(OPT) --make -o $@ $@.hs

.PHONY : $(SRC)

clean : 
	rm -f *.o *.hi *~ 



