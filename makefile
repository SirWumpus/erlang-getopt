PROJECT = egetopt
PROJECT_DESCRIPTION = POSIX style command-line option parsing.
PROJECT_VERSION = 1.1.0

include erlang.mk

nuke: distclean
	rm -rf _build *.beam example example2

examples: example example2

example: src/example.erl src/egetopt.erl
	${MAKE} ESCRIPT_NAME=example escript

example2: src/example2.erl src/egetopt.erl
	${MAKE} ESCRIPT_NAME=example2 escript

tar:
	git archive --format tar.gz --prefix ${PROJECT}/ -o ${PROJECT}-${PROJECT_VERSION}.tar.gz HEAD

