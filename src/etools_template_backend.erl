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
	BuildProject = fun(ChildParent, ChildChildren) ->
		build_project(Dir, ChildParent, ChildChildren, VarSpecs, Origin)
	end,
	BuildItem = fun(Path, Template) ->
		{filename:join(Parent, Path), Template}
	end,
	foldlwhile(fun build_project_item/3, [],
		{BuildFileName, BuildModule, BuildProject, BuildItem}, Children).

build_project_item(
	{FileNameTemplate, TemplateName = [H|_]}, Tree,
	Builders = {BuildFileName, BuildModule, _BuildProject, BuildItem}
) when is_number(H) ->
	case BuildFileName(FileNameTemplate) of
		{ok, {_, FileName}} ->
			case BuildModule(TemplateName) of
				{ok, {_, Template}} ->
					{ok, Tree ++ [BuildItem(FileName, Template)], Builders};
				Error -> Error
			end;
		Error -> Error
	end;

build_project_item(
	{ChildParent, ChildChildren}, Tree,
	Builders = {_BuildFileName, _BuildModule, BuildProject, BuildItem}
) ->
	case BuildProject(ChildParent, ChildChildren) of
		{ok, ChildTree} ->
			{ok, Tree ++ [BuildItem(Path, Template) ||
				{Path, Template} <- ChildTree], Builder};
		Error -> Error
	end.

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
	foldlwhile(fun update_vars/3, [],
		build_var_fun(TemplateDir, VarSpecs, Origin), VarNames).

update_vars(VarName, Vars, BuildVar) ->
	case BuildVar(VarName) of
		{ok, Var} -> {ok, [Var|Vars], BuildVar};
		Error -> Error
	end.

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

foldlwhile(Fun, Acc0, Tmp0, List) ->
	foldlwhile(lists:foldl(fun
		(X, {ok, Acc, Tmp}) -> Fun(X, Acc, Tmp);
		(_, Error = {error, _}) -> Error
	end, {ok, Acc0, Tmp0}, List)).

foldlwhile({ok, Acc, _F}) -> {ok, Acc};
foldlwhile(Error) -> Error.

droplast(List, Char) ->
	IsCharLast = lists:suffix([Char], List),
	if IsCharLast -> droplast(List); true -> List end.

droplast(List) -> string:left(List, length(List) - 1).
