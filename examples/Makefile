EXAMPLES = $(wildcard *.ml)
TARGETS = $(patsubst %.ml, %.bc, $(EXAMPLES))

.PHONY: all clean

all:
	@dune build $(TARGETS)

clean:
	@dune clean
