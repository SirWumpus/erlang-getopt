%%#!/usr/bin/env escript
-module(example).
-export([main/1]).

usage() ->
	io:format("usage: example [-a][-b...][-c param][-d item] ...~n"),
	io:format("-a\t\tflag~n"),
	io:format("-b\t\tcount repeated occurences~n"),
	io:format("-c param\tparameter~n"),
	io:format("-d item\t\trepeat occurences add to list~n"),
	halt(2).

main(Args) ->
	case egetopt:parse(Args, [
		{ $a, flag, opt_a },
		{ $b, count, opt_b },
		{ $c, param, opt_c },
		{ $d, list, opt_d }
	]) of
	{ok, Options, ArgsN} ->
		io:format("parsed ~p, remaining ~p~n", [Options, ArgsN]);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.
