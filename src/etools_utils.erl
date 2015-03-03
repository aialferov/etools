%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@alferov.me>
%%% @copyright (C) 2015, Anton I Alferov
%%%
%%% Created: 27 Feb 2015 by Anton I Alferov <casper@alferov.me>
%%%-------------------------------------------------------------------

-module(etools_utils).

-export([read_file/1]).
-export([split_kv/2]).

-define(LF, "\n").
-define(Comment, $#).

read_file(FileName) -> read_file(FileName, ?LF).
read_file(FileName, LineSep) ->
	case file:read_file(FileName) of
		{ok, Binary} -> read_file_binary(Binary, LineSep);
		_Error -> []
	end.

read_file_binary(Binary, LineSep) ->
	[Stripped ||
		Token <- string:tokens(binary_to_list(Binary), LineSep),
		Stripped <- [strip_after(Token, ?Comment)], Stripped =/= []
	].

split_kv(Kv, Sep) ->
	{K, [Sep|V]} = lists:splitwith(fun(X) -> X =/= Sep end, Kv), {K, V}.

strip_after(List, Char) -> lists:takewhile(fun(X) -> X =/= Char end, List).
