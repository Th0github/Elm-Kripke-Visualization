
.PHONY: all build run test clean

all: report.pdf build

build:
	stack build

run:
	stack build && stack exec myprogram

test:
	stack test --coverage

clean:
	stack clean
