%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@alferov.me>
%%% @copyright (C) 2015, Anton I Alferov
%%%
%%% Created: 24 Mar 2015 by Anton I Alferov <casper@alferov.me>
%%%-------------------------------------------------------------------

-module(etools_template_file).
-export([build/4]).

build(Dir, Name, Vars, FileName) ->
	file:write_file(FileName, format(etools_template1:build(Dir, Name, Vars))).

format(Text) -> lists:flatten(io_lib:format("~s~n", [Text])).
