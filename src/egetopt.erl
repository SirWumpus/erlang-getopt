-module(egetopt).
%-compile(export_all).
-export([parse/2]).

%%
%% @param Args
%%	List of argument strings to parse according to POSIX command-
%%	line option and argument rules.
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
%% @param Opts
%%
%%	An option specification can be one of three formats: a getopt(3)
%%	style string; a mapping of option glyph to option name and type
%%	tuple; or a tuple list of {glyph, type, name}.
%%
%% @return
%%	On succes {ok, Map_or_Plist, ArgsRemaining}; otherwise {error, Reason, Glyph}.
%%
%%
%%
parse(Args, Opts) when is_tuple(hd(Opts)) ->
	% 1.0.0 behaviour.
	case parse_opts(Args, map_opts_tuple(Opts), #{}) of
	{ok, Map, ArgsN} ->
		{ok, maps:to_list(Map), ArgsN};
	Other ->
		Other
	end;
parse(Args, Opts) when is_map(Opts) ->
	parse_opts(Args, Opts, #{});
parse(Args, Opts) ->
	parse_opts(Args, map_opts_string(Opts), #{}).

% End of arguments?
parse_opts([], _Opts, Acc) ->
	{ok, Acc, []};
% Solitaire hyphen is an argument, not an option.
parse_opts(["-" | Args], _Opts, Acc) ->
	{ok, Acc, ["-" | Args]};
% Double hyphen ends option list, remainder are arguments.
parse_opts(["--" | Args], _Opts, Acc) ->
	{ok, Acc, Args};
% Leading hyphen signals option(s).
parse_opts([ [$- | Arg] | Args], Opts, Acc) ->
	case parse_opt(Arg, Opts, Args, Acc) of
	{ok, Found, ArgsN} ->
		parse_opts(ArgsN, Opts, Found);
	Other ->
		Other
	end;
% No more options.
parse_opts(Args, _Opts, Acc) ->
	{ok, Acc, Args}.

parse_opt([], _Opts, Args, Acc) ->
	{ok, Acc, Args};
parse_opt([Ch | Chs], Opts, Args, Acc) ->
	case maps:find(Ch, Opts) of
	% -fff or -f -f -f
	{ok, {Name, flag}} ->
		% Option flag on.
		parse_opt(Chs, Opts, Args, Acc#{Name => true});

	% -p{arg}
	{ok, {Name, param}} when Chs /= [] ->
		% Set the parameter.
		parse_opt([], Opts, Args, Acc#{Name => Chs});

	% -p {arg}
	{ok, {Name, param}} when Args /= [] ->
		parse_opt(Chs, Opts, tl(Args), Acc#{Name => hd(Args)});

	% -fff or -f -f -f
	{ok, {Name, count}} ->
		% Count flag.
		Count = maps:get(Name, Acc, 0),
		parse_opt(Chs, Opts, Args, Acc#{Name => Count+1});

	% -p{arg}
	{ok, {Name, list}} when Chs /= [] ->
		% Build parameter list, reverse order.
		List = maps:get(Name, Acc, []),
		parse_opt([], Opts, Args, Acc#{Name => [Chs | List]});

	% -p {arg}
	{ok, {Name, list}} when Args /= [] ->
		% Build parameter list, reverse order.
		List = maps:get(Name, Acc, []),
		parse_opt(Chs, Opts, tl(Args), Acc#{Name => [hd(Args) | List]});

	% -p
	{ok, _} ->
		{error, "missing parameter", Ch};

	error ->
		{error, "unknown option", Ch}
	end.

%% Convert getopt(3) optstring into map of #{Glyph := {Type, Name}}.
map_opts_string(Opts) ->
	map_opts_string(Opts, #{}).
map_opts_string([], Acc) ->
	Acc;
map_opts_string([Opt, $: | Opts], Acc) ->
	Name = binary_to_atom(<<"opt_", Opt>>, latin1),
	map_opts_string(Opts, Acc#{Opt => {Name, param}});
map_opts_string([Opt, $; | Opts], Acc) ->
	Name = binary_to_atom(<<"opt_", Opt>>, latin1),
	map_opts_string(Opts, Acc#{Opt => {Name, list}});
map_opts_string([Opt, $# | Opts], Acc) ->
	Name = binary_to_atom(<<"opt_", Opt>>, latin1),
	map_opts_string(Opts, Acc#{Opt => {Name, count}});
map_opts_string([Opt | Opts], Acc) ->
	Name = binary_to_atom(<<"opt_", Opt>>, latin1),
	map_opts_string(Opts, Acc#{Opt => {Name, flag}}).

%% Convert original 1.0.0 option specification tuple into map.
map_opts_tuple(Opts) ->
	map_opts_tuple(Opts, #{}).
map_opts_tuple([], Acc) ->
	Acc;
map_opts_tuple([{Glyph, Type, Name} | Opts], Acc) ->
	map_opts_tuple(Opts, Acc#{Glyph => {Name, Type}}).
