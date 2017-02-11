%%#!/usr/bin/env escript
-module(egetopt).
%-compile(export_all).
-export([parse/2]).

%%
%% @param Args
%%	List of argument strings to parse according to POSIX command-
%%	line option and argument rules.
%%
%% @param Opts
%%	List of option tuples, [ { Glyph, OptType, Name } ].
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
	{Ch, flag, Name} ->
		%% Stop duplicates being added.
		case proplists:get_value(Name, Acc) of
		undefined ->
			parse_opt(Chs, Opts, Args, [ {Name, true} | Acc ]);
		_ ->
			parse_opt(Chs, Opts, Args, Acc)
		end;

	%% -fff or -f -f -f
	{Ch, count, Name} ->
		%% Count duplicates.
		case proplists:get_value(Name, Acc) of
		undefined ->
			parse_opt(Chs, Opts, Args, [ {Name, 1} | Acc ]);
		Value ->
			parse_opt(Chs, Opts, Args,
				lists:keyreplace(Name, 1, Acc, {Name, Value+1}))
		end;

	%% -f{arg}
	{Ch, param, Name} when Chs /= [] ->
		option_arg(Name, Chs, Args, Acc);

	%% -f {arg}
	{Ch, param, Name} when Args /= [] ->
		option_arg(Name, hd(Args), tl(Args), Acc);

	%% -f{arg} -f{arg2}
	{Ch, list, Name} when Chs /= [] ->
		option_list(Name, Chs, Args, Acc);

	%% -f {arg} -f {arg2}
	{Ch, list, Name} when Args /= [] ->
		option_list(Name, hd(Args), tl(Args), Acc);

	%% -f
	{Ch, _, _} ->
		{error, "missing argument", Ch};
	none ->
		{error, "unknown option", Ch}
	end.

option_arg(Name, Value, Args, Acc) ->
	case proplists:get_value(Name, Acc) of
	undefined ->
		{ok, [ {Name, Value} | Acc ], Args};
	_ ->
		{ok, lists:keyreplace(Name, 1, Acc, {Name, Value}), Args}
	end.

option_list(Name, Item, Args, Acc) ->
	case proplists:get_value(Name, Acc) of
	undefined ->
		{ok, [ {Name, [ Item ]} | Acc ], Args};
	Value ->
		{ok, lists:keyreplace(Name, 1, Acc, {Name, [ Item | Value ]}), Args}
	end.
