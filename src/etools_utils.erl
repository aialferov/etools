%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@alferov.me>
%%% @copyright (C) 2015, Anton I Alferov
%%%
%%% Created: 27 Feb 2015 by Anton I Alferov <casper@alferov.me>
%%%-------------------------------------------------------------------

-module(etools_utils).

-export([cmd/1]).
-export([read_file/1]).

-export([do_while/1, foldl_while/4]).

-export([drop_after/2, drop_last/2]).
-export([split_kv/2]).

-export([sub/2, sub/3]).

-define(LF, $\n).
-define(Comment, $#).

-define(PortSettings, [exit_status, stderr_to_stdout]).

cmd(Cmd) -> cmd_wait(open_port({spawn, Cmd}, ?PortSettings), []).

cmd_wait(Port, Data) -> receive
	{Port, {data, NewData}} -> cmd_wait(Port, Data ++ NewData);
	{Port, {exit_status, 0}} -> {ok, strip(Data)};
	{Port, {exit_status, Status}} -> {error, {Status, strip(Data)}}
end.

read_file(FileName) -> read_file(FileName, [?LF]).
read_file(FileName, LineSep) ->
	case file:read_file(FileName) of
		{ok, Binary} -> read_file_binary(Binary, LineSep);
		_Error -> []
	end.

read_file_binary(Binary, LineSep) ->
	[Stripped ||
		Token <- string:tokens(binary_to_list(Binary), LineSep),
		Stripped <- [drop_after(Token, ?Comment)], Stripped =/= []
	].

do_while(Funs) ->
	{_Id, Result} = hd(lists:foldl(fun do_fun/2, [], Funs)), Result.

do_fun(_FunSpec, Results = [{_Id, {error, _Reason}}|_]) -> Results;
do_fun({Id, Fun, ArgSpecs}, Results) ->
	case apply(Fun, do_args(ArgSpecs, Results)) of
		ok -> [{Id, ok}|Results];
		{ok, Result} -> [{Id, {ok, Result}}|Results];
		Error = {error, _} -> [{Id, Error}|Results]
	end.

do_args(ArgSpecs, Results) ->
	[do_arg(ArgSpec, Results) || ArgSpec <- ArgSpecs].

do_arg({Id}, Results) when is_atom(Id) ->
	case lists:keyfind(Id, 1, Results) of {Id, {ok, Result}} -> Result end;
do_arg(ArgSpec, _Results) -> ArgSpec.

foldl_while(Fun, Acc0, List, Tmp) ->
	lists:foldl(fun
		(X, {ok, Acc}) -> Fun(X, Acc, Tmp);
		(_, Error = {error, _}) -> Error
	end, {ok, Acc0}, List).

drop_after(List, Char) -> lists:takewhile(fun(X) -> X =/= Char end, List).

drop_last(List, Char) ->
	IsCharLast = lists:suffix([Char], List),
	if IsCharLast -> droplast(List); true -> List end.

droplast(List) -> string:left(List, length(List) - 1).

split_kv(Kv, Sep) ->
	{K, [Sep|V]} = lists:splitwith(fun(X) -> X =/= Sep end, Kv), {K, V}.

sub({Old, New}, Str) -> sub(Str, Old, New).
sub(Str, Old, New) -> sub(Str, Old, New, length(Old)).

sub(Str, Old, New, L) -> sub(Str, Old, New, L, string:str(Str, Old)).
sub(Str, _Old, _New, _L, 0) -> Str;
sub(Str, Old, New, OldLen, I) -> string:left(Str, I - 1) ++ New ++
	sub(string:substr(Str, I + OldLen), Old, New, OldLen).

strip(List) -> string:strip(List, right, ?LF).
