getopt
======

Parse list of strings according to POSIX command-line option and argument rules.  There is no support for GNU long option names, see [jcomellas/getopt](https://github.com/jcomellas/getopt) instead.

Data Types
----------

* Args = [ string() ]
* Glyph = alpha() | digits()
* IsFlag = boolean()
* Name = atom()
* Olist = [ Option ]
* Option = { Glyph, IsFlag, Name }
* Plist = [ { Name, Value } ]
* Value = boolean() | integer() | string()

Exports
-------

### parse(Args1, Olist) -> {ok, Plist, Args2} | {error, Reason, Glyph}

Parse list of strings according to POSIX command-line option and argument rules.  An argument that starts with a leading hyphen (-) followed by a single character, "-f".  An option is either an option-flag, "-f", or option-argument, "-x arg".  Option-flags can appear together in any order as a list, "-hfg"; an option-argument can appear at the end of list of option-flags, "-hfgx arg" or "-hfgxarg".  Options can appear in any order until a "--" argument is seen, which indicates the remainder are only arguments.


Example
-------

```
main(Args) ->
	{ok, Olist, ArgsN} = getopt:parse(Args, [
		{$f, true, foo},
		{$b, false, bar} 
	]),
	do_something(Olist),
	io:format("arguments ~p~n", [ArgsN]).
	
do_something([]) ->	
	ok;
do_something([Option | Tail]) ->
	show(Option),
	do_something(Tail).

show({foo, _}) ->
	io:format("-f~n");
show({bar, Value}) ->
	io:format("-b \"~p\"~n", [Value]).	
```

