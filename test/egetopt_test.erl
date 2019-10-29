-module(egetopt_test).
-include_lib("eunit/include/eunit.hrl").

-define(OPTIONS, [
	{ $a, flag, opt_a },
	{ $b, count, opt_b },
	{ $c, param, opt_c },
	{ $d, list, opt_d }
]).

-define(OPTMAP, #{
	$a => { opt_flag, flag },
	$b => { opt_count, count },
	$c => { opt_param, param },
	$d => { opt_list, list }
}).

-define(OPTSTRING, "ab#c:d;").

parse_test_() -> [
	?_assertMatch({error, "unknown option", $x}, egetopt:parse(["-x"], ?OPTIONS)),
	?_assertMatch({error, "unknown option", $x}, egetopt:parse(["-ax"], ?OPTIONS)),
	?_assertMatch({error, "unknown option", $x}, egetopt:parse(["-a","-x"], ?OPTIONS)),

	?_assertMatch({error, "missing parameter", $c}, egetopt:parse(["-c"], ?OPTIONS)),
	?_assertMatch({error, "missing parameter", $c}, egetopt:parse(["-ac"], ?OPTIONS)),
	?_assertMatch({error, "missing parameter", $c}, egetopt:parse(["-a","-c"], ?OPTIONS)),

	?_assertMatch({ok,[],["foo","bar"]}, egetopt:parse(["foo","bar"], ?OPTIONS)),
	?_assertMatch({ok,[],["foo","bar"]}, egetopt:parse(["--","foo","bar"], ?OPTIONS)),
	?_assertMatch({ok,[],["-", "foo","bar"]}, egetopt:parse(["-","foo","bar"], ?OPTIONS)),
	?_assertMatch({ok,#{},["-", "foo","bar"]}, egetopt:parse(["-","foo","bar"], ?OPTSTRING)),

	?_assertMatch({ok,[{opt_a,true}],[]}, egetopt:parse(["-a"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_a,true}],[]}, egetopt:parse(["-aa"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_a,true}],[]}, egetopt:parse(["-a","-a"], ?OPTIONS)),
	?_assertMatch({ok,#{opt_a := true},[]}, egetopt:parse(["-a","-a"], ?OPTSTRING)),

	?_assertMatch({ok,[{opt_b,1}],[]}, egetopt:parse(["-b"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_b,2}],[]}, egetopt:parse(["-bb"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_a,true},{opt_b,3}],[]}, egetopt:parse(["-bab","-b"], ?OPTIONS)),
	?_assertMatch({ok,#{opt_a := true,opt_b := 3},[]}, egetopt:parse(["-bab","-b"], ?OPTSTRING)),

	?_assertMatch({ok,[{opt_c,"hello"}],[]}, egetopt:parse(["-chello"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_c,"hello"}],[]}, egetopt:parse(["-c","hello"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_c,"world"}],[]}, egetopt:parse(["-chello","-c","world"], ?OPTIONS)),
	?_assertMatch({ok,#{opt_c := "world"},[]}, egetopt:parse(["-chello","-c","world"], ?OPTSTRING)),

	?_assertMatch({ok,[{opt_d,["hello"]}],[]}, egetopt:parse(["-dhello"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_d,["hello"]}],[]}, egetopt:parse(["-d","hello"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_d,["world","hello"]}],[]}, egetopt:parse(["-dhello","-d","world"], ?OPTIONS)),
	?_assertMatch({ok,#{opt_d := ["world","hello"]},[]}, egetopt:parse(["-dhello","-d","world"], ?OPTSTRING)),

	?_assertMatch({ok,[{opt_a,true}],["foo","bar"]}, egetopt:parse(["-a","foo","bar"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_a,true}],["-b"]}, egetopt:parse(["-a","--","-b"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_a,true}],["-", "-b"]}, egetopt:parse(["-a","-","-b"], ?OPTIONS)),
	?_assertMatch({ok,#{opt_a := true},["-", "-b"]}, egetopt:parse(["-a","-","-b"], ?OPTSTRING)),

	?_assertMatch({ok,[{opt_c,"foo"}],["bar"]}, egetopt:parse(["-c","foo","bar"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_b,1},{opt_c,"--"}],["baka"]}, egetopt:parse(["-c","--","-b","baka"], ?OPTIONS)),
	?_assertMatch({ok,[{opt_b,1},{opt_c,"-"}],["baka"]}, egetopt:parse(["-c","-","-b","baka"], ?OPTIONS)),
	?_assertMatch({ok,#{opt_b := 1,opt_c := "-"},["baka"]}, egetopt:parse(["-c","-","-b","baka"], ?OPTSTRING)),

	?_assertMatch(
		{ok,#{opt_flag:=true,opt_count:=3,opt_param:="replace",opt_list:=["two","one"]},["hey","you"]},
		egetopt:parse(["-aa","-b","-bb","-cfoo","-done","-c","replace","-d","two","hey","you"], ?OPTMAP)
	)
].
