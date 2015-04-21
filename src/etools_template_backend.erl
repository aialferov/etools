%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@alferov.me>
%%% @copyright (C) 2015, Anton I Alferov
%%%
%%% Created: 15 Mar 2015 by Anton I Alferov <casper@alferov.me>
%%%-------------------------------------------------------------------

-module(etools_template_backend).
-export([build_project/3, build_template/3]).

-define(Utils, etools_utils).

-define(ProjectTemplateFile(Dir, Name), filename:join([Dir, "projects", Name])).
-define(ModuleTemplateFile(Dir, Name), filename:join([Dir, "modules", Name])).

-define(Var(Name), "$(" ++ Name ++ ")").
-define(VarRx, "\\$\\(([A-Za-z0-9-_]+)\\)").
-define(VarRxOpts, [global, {capture, all_but_first, list}]).

-define(Result(Name, Value), {?Var(Name), Value}).

build_project(Dir, Name, VarSpecs) ->
	case file:consult(FileName = ?ProjectTemplateFile(Dir, Name)) of
		{ok, Children} -> build_project(Dir, ".", Children, VarSpecs, FileName);
		{error, Reason} -> {error, {Name, file, Dir, Reason}}
	end.

build_project(Dir, Parent, Children, VarSpecs, Origin) ->
	BuildFileName = fun(FileNameTemplate) ->
		build_template(Dir, "", FileNameTemplate, VarSpecs, Origin)
	end,
	BuildModule = fun(TemplateName) ->
		build_template(Dir, TemplateName, VarSpecs, Origin)
	end,
	BuildProject = fun({ChildParent, ChildChildren}) ->
		build_project(Dir, ChildParent, ChildChildren, VarSpecs, Origin)
	end,
	BuildItem = fun(Path, Template) ->
		{filename:join(Parent, Path), Template}
	end,
	foldlwhile(fun build_project_item/3, [], Children,
		{BuildFileName, BuildModule, BuildProject, BuildItem}).

build_project_item(
	{FileNameTemplate, TemplateName = [H|_]}, Tree,
	{BuildFileName, BuildModule, _BuildProject, BuildItem}
) when is_number(H) -> dowhile([
	{filename, BuildFileName, [FileNameTemplate]},
	{template, BuildModule, [TemplateName]},
	{tree, fun({_, FileName}, {_, Template}) ->
		{ok, Tree ++ [BuildItem(FileName, Template)]}
	end, [{filename}, {template}]}
]);

build_project_item(
	{ChildParent, ChildChildren}, Tree,
	{_BuildFileName, _BuildModule, BuildProject, BuildItem}
) -> dowhile([
	{project, BuildProject, [{ChildParent, ChildChildren}]},
	{tree, fun(ChildTree) ->
		{ok, Tree ++ [BuildItem(Path, Template) || {Path, Template} <- ChildTree]}
	end, [{project}]}
]).

build_template(Dir, Name, VarSpecs) ->
	build_template(Dir, Name, VarSpecs, Dir).

build_cmd(Name, Cmd, Origin) ->
	case ?Utils:cmd(Cmd) of
		{ok, Value} -> {ok, ?Result(Name, Value)};
		{error, {_Code, Reason}} -> {error, {Name, cmd, Origin, Reason}}
	end.

build_value(Name, [], Origin) -> {error, {Name, value, Origin, empty}};
build_value(Name, Value, _Origin) -> {ok, ?Result(Name, Value)}.

build_template(Dir, Name, VarSpecs, Origin) ->
	case file:read_file(FileName = ?ModuleTemplateFile(Dir, Name)) of
		{ok, Binary} ->
			Content = droplast(binary_to_list(Binary), $\n),
			build_template(Dir, Name, Content, VarSpecs, FileName);
		{error, Reason} -> {error, {Name, file, Origin, Reason}}
	end.

build_template(Dir, Name, Content, VarSpecs, Origin) ->
	case build_vars(Dir, read_var_names(Content), VarSpecs, Origin) of
		{ok, Vars} -> {ok, ?Result(Name, build_template(Content, Vars))};
		Error -> Error
	end.

build_template(Content, Vars) -> lists:foldl(fun ?Utils:sub/2, Content, Vars).

build_vars(TemplateDir, VarNames, VarSpecs, Origin) ->
	foldlwhile(fun build_var_into/3, [], VarNames,
		build_var_fun(TemplateDir, VarSpecs, Origin)).

build_var_into(VarName, Vars, BuildVarFun) -> dowhile([
	{build, BuildVarFun, [VarName]},
	{add, fun(Var) -> {ok, [Var|Vars]} end, [{build}]}
]).

build_var_fun(TemplateDir, VarSpecs, Origin) ->
	fun(VarName) -> build_var(TemplateDir, VarName, VarSpecs, Origin) end.

build_var(TemplateDir, VarName, VarSpecs, Origin) ->
	case lists:keyfind(VarName, 1, VarSpecs) of
		{VarName, {cmd, Cmd}} -> build_cmd(VarName, Cmd, Origin);
		{VarName, {value, Value}} -> build_value(VarName, Value, Origin);
		false -> build_template(TemplateDir, VarName, VarSpecs, Origin)
	end.

read_var_names(Content) ->
	case re:run(Content, ?VarRx, ?VarRxOpts) of
		{match, Captured} -> lists:usort([X || [X] <- Captured]);
		nomatch -> []
	end.

dowhile([]) -> false;
dowhile(Funs) -> {_Id, Result} = hd(lists:foldl(fun do/2, [], Funs)), Result.

do(_FunSpec, Results = [{_Id, {error, _Reason}}|_]) -> Results;
do({Id, Fun, ArgSpecs}, Results) ->
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

foldlwhile(Fun, Acc0, List, Tmp) ->
	lists:foldl(fun
		(X, {ok, Acc}) -> Fun(X, Acc, Tmp);
		(_, Error = {error, _}) -> Error
	end, {ok, Acc0}, List).

droplast(List, Char) ->
	IsCharLast = lists:suffix([Char], List),
	if IsCharLast -> droplast(List); true -> List end.

droplast(List) -> string:left(List, length(List) - 1).
