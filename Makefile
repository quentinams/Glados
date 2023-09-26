all: run

clean:
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