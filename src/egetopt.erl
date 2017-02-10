%%#!/usr/bin/env escript
-module(getopt).
%-compile(export_all).
-export([parse/2]).

%%
%% @param Args
%%	List of argument strings to parse according to POSIX command-
%%	line option and argument rules.
%%
%% @param Opts
%%	List of option tuples, [ { Glyph, IsFlag, Name } ].
%%
%%	An argument that starts with a leading hyphen (-) followed by
%%	a single character, "-f".  An option is either an option-flag,
%%	"-f", or option-argument, "-x arg".  Option-flags can appear
%%	together in any order as a list, "-hfg"; an option-argument can
%%	appear at the end of list of option-flags, "-hfgx arg" or
%%	"-hfgxarg".  Options can appear in any order until a "--"
%%	argument is seen, which indicates the remainder are only
%%	arguments.
%%
%% @return
%%	On succes {ok, Plist, Args2}; otherwise {error, Reason, Glyph}.
%%
parse(Args, Opts) ->
	parse(Args, Opts, []).

%% End of arguments?
parse([], _Opts, Acc) ->
	{ok, Acc, []};
%% Solitaire hyphen is an argument, not an option.
parse(["-" | Args], _Opts, Acc) ->
	{ok, Acc, ["-" | Args]};
%% Double hyphen ends option list, remainder are arguments.
parse(["--" | Args], _Opts, Acc) ->
	{ok, Acc, Args};
%% Leading hyphen signals option(s).
parse([ [$- | Arg] | Args], Opts, Acc) ->
	case parse_opt(Arg, Opts, Args, Acc) of
	{ok, Found, ArgsN} ->
		parse(ArgsN, Opts, Found);
	Other ->
		Other
	end;
%% No more options.
parse(Args, _Opts, Acc) ->
	{ok, Acc, Args}.

parse_opt([], _Opts, Args, Acc) ->
	{ok, Acc, Args};
parse_opt([Ch | Chs], Opts, Args, Acc) ->
	case proplists:lookup(Ch, Opts) of
	%% -fgh
	{_, true, Name} ->
		parse_opt(Chs, Opts, Args, [ {Name, true} | Acc ]);
	%% -f{arg}
	{_, false, Name} when Chs /= [] ->
		{ok, [ {Name, Chs} | Acc ], Args};
	%% -f {arg}
	{_, false, Name} when Args /= [] ->
		{ok, [ {Name, hd(Args)} | Acc ], tl(Args)};
	%% -f
	{_, false, _} ->
		{error, "missing argument", Ch};
	none ->
		{error, "unknown option", Ch}
	end.

