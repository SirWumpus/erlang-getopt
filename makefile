A = .a
O = .o
B = .beam
E =
.SUFFIXES : .h .c .i $O $E .hrl .erl .beam .sh

PROJ		:= egetopt

BIN		:= _build/default/bin
ELIB		:= _build/default/lib
EBIN		:= ${ELIB}/${PROJ}/ebin
ERLC_FLAGS	:= -o${EBIN}

$E$B:
	erlc ${ERLC_FLAGS} $@

all:
	rebar3 compile

clean:
	-rm -rf src/*$B ./*$B *dump

distclean: clean
	-rm -rf _build cover ebin

tar:
	git archive --format tar.gz --prefix ${PROJ}/ -o ${PROJ}.tar.gz HEAD

examples:
	rebar3 as example1 escriptize
	rebar3 as example2 escriptize
	rebar3 as example3 escriptize

test: dialyzer unit

dialyzer:
	rebar3 dialyzer

unit:
	rebar3 eunit --cover
	rebar3 cover
