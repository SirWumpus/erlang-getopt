-module(egetopt).
%-compile(export_all).
-export([parse/2, to_map/2]).

-type args()		:: [string()].
-type glyph()		:: 16#21..16#7E.  % ASCII printable character, except hyphen.
-type optname()		:: atom().
-type opttype()		:: count | flag | list | param.
-type optarg()		:: undefined | boolean() | non_neg_integer() | string() | [string()].
-type optlist()		:: [{glyph(), opttype(), optname()}].
-type optmap()		:: #{glyph() => {optname(), opttype()}}.
-type opts()		:: string() | optmap() | optlist().

-type ret_plist()	:: [{optname(), optarg()}].
-type ret_map()		:: #{optname() => optarg()}.
-type ret_err()		:: {error, string(), glyph()}.
-type ret_ok(Ret)	:: {ok, Ret, args()}.

-export_type([
	args/0,
	glyph/0,
	optname/0,
	opttype/0,
	optarg/0,
	optlist/0,
	optmap/0,
	opts/0,
	ret_plist/0,
	ret_map/0,
	ret_err/0,
	ret_ok/1
]).

%%
%% Parse a list of strings according to POSIX command-line option and
%% argument rules.
%%
%% @param Args
%%	An argument that starts with a leading hyphen (`-') followed by
%%	a single character, `-f'.  An option is either an option-flag,
%%	`-f', or option-argument, `-x arg'.  Option-flags can appear
%%	together in any order as a list, `-hfg'; an option-argument can
%%	appear at the end of list of option-flags, `-hfgx arg' or
%%	`-hfgxarg'.  Options can appear in any order until a `--'
%%	argument is seen, which indicates the remainder are only
%%	arguments.
%%
%% @param Opts
%%	An option specification can be one of three formats: a getopt(3)
%%	style string; a mapping of option glyph to option name and type
%%	tuple; or a tuple list of `{glyph, type, name}'.
%%
%% @return
%%	On success `{ok, Plist, ArgsRemaining}'; otherwise `{error, Reason, Glyph}'.
%%
-spec parse(args(), opts()) -> ret_ok(ret_plist()) | ret_err().
parse(Args, Opts) ->
	case to_map(Args, Opts) of
	{ok, Options, ArgsN} ->
		{ok, maps:to_list(Options), ArgsN};
	Other ->
		Other
	end.

-spec to_map(args(), opts()) -> ret_ok(ret_map()) | ret_err().
to_map(Args, Opts) when is_map(Opts) ->
	parse_opts(Args, Opts, #{});
to_map(Args, [Opt | Opts]) when is_tuple(Opt) ->
	% 1.0.0 behaviour.
	to_map(Args, map_opts_tuple([Opt | Opts]));
to_map(Args, Opts) ->
	to_map(Args, map_opts_string(Opts)).

-spec parse_opts(args(), optmap(), ret_map()) -> ret_ok(ret_map()) | ret_err().
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

-spec parse_opt(args(), optmap(), args(), ret_map()) -> ret_ok(ret_map()) | ret_err().
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

%% Convert getopt(3) optstring into map of #{Glyph := {Name, Type}}.
-spec map_opts_string(string()) -> optmap().
map_opts_string(Opts) ->
	map_opts_string(Opts, #{}).

-spec map_opts_string(string(), optmap()) -> optmap().
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

% Convert original 1.0.0 option tuple specification into map.
-spec map_opts_tuple(optlist()) -> optmap().
map_opts_tuple(Opts) ->
	maps:from_list([{Glyph, {Name, Type}} || {Glyph, Type, Name} <- Opts]).
