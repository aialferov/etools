%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@alferov.me>
%%% @copyright (C) 2015, Anton I Alferov
%%%
%%% Created: 24 Mar 2015 by Anton I Alferov <casper@alferov.me>
%%%-------------------------------------------------------------------

-module(etools_template_project).
-export([build/3]).

-include("etools_template_config.hrl").

-define(FileBackend, etools_template_file).
-define(Utils, etools_utils).

-define(Pwd, ".").

build(Dir, Name, VarSpecs) ->
	case file:consult(FileName = ?ProjectTemplate(Dir, Name)) of
		{ok, Children} -> build_tree(Dir, ?Pwd, Children, VarSpecs, FileName);
		{error, Reason} -> {error, {Name, file, Dir, Reason}}
	end.

build_tree(Dir, Parent, Children, VarSpecs, Origin) ->
	BuildFileName = fun(FileNameTemplate) ->
		?FileBackend:build_from_text(Dir, FileNameTemplate, VarSpecs, Origin)
	end,
	BuildFile = fun(Name) ->
		?FileBackend:build_from_file(Dir, Name, VarSpecs, Origin)
	end,
	BuildTree = fun(ChildParent, ChildChildren) ->
		build_tree(Dir, ChildParent, ChildChildren, VarSpecs, Origin)
	end,
	BuildTreeItem = fun(Path, File) ->
		{filename:join(Parent, Path), File}
	end,
	?Utils:foldl_while(fun build_tree_item/3, [], Children,
		{BuildFileName, BuildFile, BuildTree, BuildTreeItem}).

build_tree_item(
	{FileNameTemplate, TemplateName = [H|_]}, Tree,
	{BuildFileName, BuildFile, _BuildTree, BuildTreeItem}
) when is_number(H) ->
	CompleteTree = fun(FileName, File) ->
		{ok, Tree ++ [BuildTreeItem(FileName, File)]}
	end,
	?Utils:do_while([
		{filename, BuildFileName, [FileNameTemplate]},
		{file, BuildFile, [TemplateName]},
		{tree, CompleteTree, [{filename}, {file}]}
	]);

build_tree_item(
	{ChildParent, ChildChildren}, Tree,
	{_BuildFileName, _BuildFile, BuildTree, BuildTreeItem}
) ->
	CompleteTree = fun(ChildTree) ->
		{ok, Tree ++ [BuildTreeItem(Path, File) || {Path, File} <- ChildTree]}
	end,
	?Utils:do_while([
		{child_tree, BuildTree, [ChildParent, ChildChildren]},
		{tree, CompleteTree, [{child_tree}]}
	]).
