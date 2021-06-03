%%#!/usr/bin/env escript
%%
%% @private Example using an option tuple list.
%%
-module(example3).
-export([main/1]).

-spec usage() -> none().
usage() ->
	io:format("usage: example [-a][-b...][-c param][-d item] ...~n"),
	io:format("-a\t\tset flag true~n"),
	io:format("-b\t\tcount repeated occurences~n"),
	io:format("-c param\tset parameter~n"),
	io:format("-d item\t\tadd item to list~n"),
	halt(2).

-spec options() -> egetopt:optlist().
options() -> [
	{ $a, flag,  option_a },
	{ $b, count, option_b },
	{ $c, param, option_c },
	{ $d, list,  option_d }
].

-spec main([string()]) -> ok.
main(Args) ->
	case egetopt:parse(Args, options()) of
	{ok, Options, ArgsN} ->
		io:format("parsed ~p, remaining ~p~n", [Options, ArgsN]);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.
