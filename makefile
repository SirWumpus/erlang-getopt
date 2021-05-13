PROJECT = egetopt
PROJECT_DESCRIPTION = POSIX style command-line option parsing.
PROJECT_VERSION = 1.1.0
COVER=1

include erlang.mk

all:: examples

distclean::
	-rm example[123]

examples: example1 example2 example3

example1: src/example1.erl src/egetopt.erl
	${MAKE} ESCRIPT_NAME=example1 escript

example2: src/example2.erl src/egetopt.erl
	${MAKE} ESCRIPT_NAME=example2 escript

example3: src/example3.erl src/egetopt.erl
	${MAKE} ESCRIPT_NAME=example3 escript

tar:
	git archive --format tar.gz --prefix ${PROJECT}/ -o ${PROJECT}-${PROJECT_VERSION}.tar.gz HEAD
