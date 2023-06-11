# Compiler and options
GHC = ghc
GHC_FLAGS = -Wall

# Source files
SRC_FILES = main.hs GameStateOp.hs GameStateHistory.hs Field.hs RoseModule.hs Move.hs Player.hs Util.hs Board.hs

# Output executable
OUTPUT = main

CLEAN_FILES = $(OUTPUT) $(SRC_FILES:.hs=.hi) $(SRC_FILES:.hs=.o)

.PHONY: all clean clean-all

all: $(OUTPUT)

$(OUTPUT): $(SRC_FILES)
	$(GHC) $(GHC_FLAGS) -o $@ $^

clean:
	rm -f $(OUTPUT)

clean-all:
	rm -f $(CLEAN_FILES)