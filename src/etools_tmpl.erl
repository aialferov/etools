-module(etools_tmpl).
-export([make/2]).

-define(TmplExt, ".tmpl").
-define(TmplHeaderExt, ".tmpl.h").

-define(TmplDir, filename:join(["priv", "tmpl"])).

-define(Vars, [
	{author_name, "getent passwd `whoami` | cut -d: -f5 | tr -d ,"},
	{author_host, "echo `whoami`@`hostname`"},
	{date, "date \"+%d %b %Y\""},
	{year, "date +%Y"}
]).

make(Name, Vars) ->
	from_file(filename:join(?TmplDir, Name ++ ?TmplExt), vars(Vars)).

vars(Vars) -> headers(
	[{var_name(Name), Value} || {Name, Value} <- Vars] ++
	[{var_name(Name), cmd(Command)} || {Name, Command} <- ?Vars]
).

headers(Vars) -> Vars ++ [
	{var_name(filename:basename(FileName, ?TmplHeaderExt)),
	 from_file(filename:join(?TmplDir, FileName), Vars)} ||
	FileName <- lists:reverse(filelib:wildcard([$*|?TmplHeaderExt], ?TmplDir))
].

from_file(File, Vars) ->
	{ok, Binary} = file:read_file(File),
	lists:foldl(fun sub/2, binary_to_list(Binary), Vars).

var_name(VarName) when is_atom(VarName) -> var_name(atom_to_list(VarName));
var_name(VarName) -> [$$|VarName].

cmd(Command) -> string:strip(os:cmd(Command), right, $\n).

sub({Old, New}, Str) -> sub(Str, Old, New, string:str(Str, Old)).
sub(Str, _, _, 0) -> Str;
sub(Str, Old, New, I) -> sub({Old, New}, string:left(Str, I - 1) ++
	New ++ string:right(Str, string:len(Str) - I + 1 - string:len(Old))).
