%%#!/usr/bin/env escript
-module(example).
-export([main/1]).

usage() ->
	io:format("usage: example [-a][-b string] ...~n"),
	halt(2).

main(Args) ->
	case getopt:parse(Args, [
		{ $a, true, opt_a },
		{ $b, false, opt_b }
	]) of
	{ok, Options, ArgsN} ->
		io:format("parsed ~p, remaining ~p~n", [Options, ArgsN]);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.
