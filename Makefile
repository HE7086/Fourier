SRC := src/Main.hs src/Form.hs src/Jsonparser.hs src/Settings.hs
BIN := Main
FLAGS := -O2 -threaded -Wno-deprecations
SRCPATH := $(PWD)/data
OUTPATH := $(PWD)/out

.PHONY: all clean doc gif png evo

all:
	ghc $(FLAGS) $(SRC) -o $(BIN)

clean: 
	rm -f $(BIN) src/*.o src/*.hi README.pdf

doc:
	pandoc -s -o README.pdf README.md

gif: 
	$(PWD)/$(BIN) gif $(SRCPATH) $(OUTPATH) +RTS -N

png: 
	$(PWD)/$(BIN) png $(SRCPATH) $(OUTPATH) +RTS -N

evo: 
	$(PWD)/$(BIN) evo $(SRCPATH) $(OUTPATH) +RTS -N
