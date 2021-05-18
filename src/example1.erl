%%#!/usr/bin/env escript
-module(example1).
-export([main/1]).

usage() ->
	io:format("usage: example1 [-a][-b...][-c param][-d item] ...~n"),
	io:format("-a\t\tset flag true~n"),
	io:format("-b\t\tcount repeated occurences~n"),
	io:format("-c param\tset parameter~n"),
	io:format("-d item\t\tadd item to list~n"),
	halt(2).

options() ->
	"ab#c:d;".

main(Args) ->
	case opts:to_map(Args, options()) of
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage();
	{ok, _Options, ArgsN} ->
		io:format("parsed ~p, opt_b again is ~p, remaining ~p~n", [opts:dump(), opts:get(opt_b), ArgsN])
	end.
