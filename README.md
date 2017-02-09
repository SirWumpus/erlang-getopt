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

Copyright
---------

Copyright 2017 by Anthony Howe.  All rights reserved.

MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
