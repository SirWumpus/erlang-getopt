%%#!/usr/bin/env escript
%%
%% @private Example using an option map.
%%
-module(example2).
-export([main/1]).

-spec usage() -> none().
usage() ->
	io:format("usage: example2 [-a][-b...][-c param][-d item] ...~n"),
	io:format("-a\t\tset flag true~n"),
	io:format("-b\t\tcount repeated occurences~n"),
	io:format("-c param\tset parameter~n"),
	io:format("-d item\t\tadd item to list~n"),
	halt(2).

-spec options() -> egetopt:optmap().
options() -> #{
	$a => {some_a, flag},
	$b => {some_b, count},
	$c => {some_c, param},
	$d => {some_d, list}
}.

-spec main([string()]) -> ok.
main(Args) ->
	case egetopt:parse(Args, options()) of
	{ok, Options, ArgsN} ->
		io:format("parsed ~p, remaining ~p~n", [Options, ArgsN]);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.
