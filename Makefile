OUTFILE = gladdos

MAIN_SOURCE = app/Main.hs
OTHER_SOURCES = $(wildcard src/*.hs)

GHC_OPTS = -O2 -outputdir obj -isrc -iapp


all: $(OUTFILE)

$(OUTFILE): $(MAIN_SOURCE) $(OTHER_SOURCES)
	ghc $(GHC_OPTS) -o $(OUTFILE) $(MAIN_SOURCE)

clean:
	rm -f $(OUTFILE)
	rm -rf obj
	@stack clean

fclean: clean
	@stack clean --full

re: fclean all

build:
	@stack build

run:
	@stack run

run-test:
	@stack test