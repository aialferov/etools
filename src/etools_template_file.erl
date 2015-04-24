%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@alferov.me>
%%% @copyright (C) 2015, Anton I Alferov
%%%
%%% Created: 15 Mar 2015 by Anton I Alferov <casper@alferov.me>
%%%-------------------------------------------------------------------

-module(etools_template_file).

-export([build_from_file/4]).
-export([build_from_text/4]).

-include("etools_template_config.hrl").

-define(Utils, etools_utils).

-define(Var(Name, Value), {"$(" ++ Name ++ ")", Value}).
-define(VarRx, "\\$\\(([A-Za-z0-9-_]+)\\)").
-define(VarRxOpts, [global, {capture, all_but_first, list}]).

build_from_file(Dir, Name, VarSpecs, Origin) ->
	case file:read_file(FileName = ?FileTemplate(Dir, Name)) of
		{ok, Binary} ->
			Text = ?Utils:drop_last(binary_to_list(Binary), $\n),
			build_from_text(Dir, Text, VarSpecs, FileName);
		{error, Reason} -> {error, {Name, file, Origin, Reason}}
	end.

build_from_text(Dir, Text, VarSpecs, Origin) ->
	case build_vars(Dir, read_var_names(Text), VarSpecs, Origin) of
		{ok, Vars} -> {ok, lists:foldl(fun ?Utils:sub/2, Text, Vars)};
		Error -> Error
	end.

build_vars(TemplateDir, VarNames, VarSpecs, Origin) ->
	?Utils:foldl_while(fun build_var_into/3, [], VarNames,
		build_var_fun(TemplateDir, VarSpecs, Origin)).

build_var_into(VarName, Vars, BuildVarFun) -> ?Utils:do_while([
	{build, BuildVarFun, [VarName]},
	{add, fun(VarValue) -> {ok, [?Var(VarName, VarValue)|Vars]} end, [{build}]}
]).

build_var_fun(TemplateDir, VarSpecs, Origin) ->
	fun(VarName) -> build_var(TemplateDir, VarName, VarSpecs, Origin) end.

build_var(TemplateDir, VarName, VarSpecs, Origin) ->
	case lists:keyfind(VarName, 1, VarSpecs) of
		{VarName, {cmd, Cmd}} -> build_cmd(VarName, Cmd, Origin);
		{VarName, {value, Value}} -> build_value(VarName, Value, Origin);
		false -> build_from_file(TemplateDir, VarName, VarSpecs, Origin)
	end.

build_cmd(Name, Cmd, Origin) ->
	case ?Utils:cmd(Cmd) of
		{ok, Value} -> {ok, Value};
		{error, {_Code, Reason}} -> {error, {Name, cmd, Origin, Reason}}
	end.

build_value(Name, [], Origin) -> {error, {Name, value, Origin, empty}};
build_value(_Name, Value, _Origin) -> {ok, Value}.

read_var_names(Content) ->
	case re:run(Content, ?VarRx, ?VarRxOpts) of
		{match, Captured} -> lists:usort([X || [X] <- Captured]);
		nomatch -> []
	end.
