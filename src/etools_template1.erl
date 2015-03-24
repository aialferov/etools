%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@alferov.me>
%%% @copyright (C) 2015, Anton I Alferov
%%%
%%% Created: 15 Mar 2015 by Anton I Alferov <casper@alferov.me>
%%%-------------------------------------------------------------------

-module(etools_template1).
-export([build/3]).

-define(Utils, etools_utils).

-define(TemplateFile(Dir, Name), filename:join([Dir, Name])).

-define(Var(Name), "$(" ++ Name ++ ")").
-define(VarRx, "\\$\\(([A-Za-z0-9-_]+)\\)").
-define(VarRxOpts, [global, {capture, all_but_first, list}]).

build(Dir, Name, Vars) ->
	case build_template(Dir, Name, Vars) of
		{true, {_Var, Value}} -> Value;
		false -> []
	 end.

build_cmd(Name, Cmd) -> build_value(Name, ?Utils:cmd(Cmd)).

build_value(_Name, []) -> false;
build_value(Name, Value) -> {true, {?Var(Name), Value}}.

build_template(Dir, Name, Vars) ->
	case file:read_file(?TemplateFile(Dir, Name)) of
		{ok, Binary} ->
			Content = droplast(binary_to_list(Binary), $\n),
			build_value(Name, lists:foldl(fun ?Utils:sub/2, Content,
				select_vars(Dir, read_var_names(Content), Vars)));
		_Error -> false
	end.

select_vars(Dir, Names, Vars) ->
	lists:filtermap(fun(Name) -> select_var(Dir, Name, Vars) end, Names).

select_var(Dir, Name, Vars) ->
	case lists:keyfind(Name, 1, Vars) of
		{Name, {cmd, Cmd}} -> build_cmd(Name, Cmd);
		{Name, {value, Value}} -> build_value(Name, Value);
		false -> build_template(Dir, Name, Vars)
	end.

read_var_names(Content) ->
	case re:run(Content, ?VarRx, ?VarRxOpts) of
		{match, Captured} -> lists:usort([X || [X] <- Captured]);
		nomach -> []
	end.

droplast(List, Char) ->
	IsCharLast = string:right(List, 1) == [Char],
	if IsCharLast -> droplast(List); true -> List end.

droplast(List) -> string:left(List, length(List) - 1).
