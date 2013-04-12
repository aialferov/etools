%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 23 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(emake).
-export([all/0, clean/0, deepclean/0]).

-include_lib("kernel/include/file.hrl").

-define(ConfigFile, "Emakefile.em").

-define(AppExt, ".app").
-define(BinExt, ".beam").
-define(SrcExt, ".erl").

-define(DepsDir, "deps").
-define(BinDir, "ebin").
-define(SrcDir, "src").
-define(IncludeDir, "include").
-define(ExamplesDir, "examples").

-define(CompileOptions(OutDir, IncludeDirs),
	[report_errors, report_warnings, {outdir, OutDir}] ++
	[{i, IncludeDir} || IncludeDir <- IncludeDirs]
).

all() ->
	{IncludeDepPaths, DepPaths} = read_deps(),
	IncludePaths = [filename:join(DepPath, ?IncludeDir) ||
		DepPath <- DepPaths ++ IncludeDepPaths],
	[build(DepPath, filename:join([?DepsDir, filename:basename(DepPath),
		?BinDir]), IncludePaths) || DepPath <- DepPaths],
	[copy_app(DepPath, filename:join([?DepsDir,
		filename:basename(DepPath), ?BinDir])) || DepPath <- DepPaths],
	build(".", ?BinDir, IncludePaths ++ [?IncludeDir]).

clean() -> clean(".").

deepclean() -> deepclean(file:list_dir(?ExamplesDir)), clean().
deepclean({ok, ExampleDirs}) ->
	[clean(?ExamplesDir ++ "/" ++ Dir) || Dir <- ExampleDirs];
deepclean({error, _Reason}) -> ok.

clean(Path) ->
	os_cmd("rm -rf " ++ Path ++ "/" ++ ?DepsDir),
	os_cmd("rm -f " ++ Path ++ "/" ++ ?BinDir ++ "/*.beam"),
	os_cmd("rmdir --ignore-fail-on-non-empty " ++ Path ++ "/" ++ ?BinDir).

os_cmd(Cmd) -> io:format("~p~n",
	[{case os:cmd(Cmd) of [] -> ok; X -> X end, Cmd}]).

read_deps() ->
	{RawIncludeDeps, RawDeps} = lists:partition(fun({_, {_, Flags}}) ->
		lists:member(include, Flags) end, lists:flatten(read_deps("."))),
	Deps = uniq(RawDeps),
	{normalize_deps(substract(RawIncludeDeps, Deps)), normalize_deps(Deps)}.

read_deps(ProjectPath) -> read_deps(file:consult(
	filename:join(ProjectPath, ?ConfigFile)), ProjectPath).
read_deps({ok, Deps}, ProjectPath) ->
	[read_deps(dep_abs_path(Dep, ProjectPath)) || Dep <- Deps] ++
		[{dep_id(Dep), prepare_dep(Dep, ProjectPath)} || Dep <- Deps];
read_deps(false, _) -> [];
read_deps({error, enoent}, _) -> [].

normalize_deps(Deps) -> [DepPath || {_, {DepPath, _}} <- Deps].

dep_id({DepRelPath, _Options}) -> filename:basename(DepRelPath).

prepare_dep(Dep = {_DepRelPath, Options}, ProjectPath) ->
	{dep_abs_path(Dep, ProjectPath), [Use || {use, Use} <- Options]}.

dep_abs_path({DepRelPath, Options}, ProjectPath) ->
	case lists:keyfind(git, 1, Options) of
		{git, DepGit} -> load_dep(filename:absname(
			DepRelPath, ProjectPath), DepGit);
		false -> filename:absname(DepRelPath, ProjectPath)
	end.

load_dep(DepPath, DepGit) ->
	load_dep(file:read_file_info(DepPath), DepPath, DepGit).
load_dep({ok, _}, DepPath, _) -> DepPath;
load_dep(_, DepPath, DepGit) ->
	os_cmd("git clone " ++ DepGit ++ " " ++ DepPath), DepPath.

build(Path, OutPath, IncludePaths) ->
	SrcPath = filename:join(Path, ?SrcDir),
	build(file:list_dir(SrcPath), Path, SrcPath, OutPath, IncludePaths).
build({ok, SrcFiles}, Path, SrcPath, OutPath, IncludePaths) ->
	io:format("Building: ~p~n", [Path]),
	SrcMods = [filename:join(SrcPath, filename:rootname(File)) ||
		File <- SrcFiles, filename:extension(File) == ?SrcExt],
	[compile_mod(SrcMod, OutPath, IncludePaths) || SrcMod <- SrcMods], ok;
build({error, enoent}, Path, _, _, _) ->
	io:format("Nothing to do in ~p~n", [Path]).

compile_mod(SrcMod, OutPath, IncludePaths) ->
	BinFile = filename:join(OutPath, filename:basename(SrcMod) ++ ?BinExt),
	compile_mod(file:read_file_info(BinFile),
		file:read_file_info(SrcMod ++ ?SrcExt), SrcMod, OutPath, IncludePaths).

compile_mod(
	{ok, #file_info{atime = BinTime}}, {ok, #file_info{atime = SrcTime}},
	SrcMod, OutPath, IncludePaths
) when SrcTime > BinTime -> c_mod(SrcMod, OutPath, IncludePaths);

compile_mod(
	{ok, #file_info{atime = BinTime}}, {ok, _},
	SrcMod, OutPath, IncludePaths
) ->
	case lists:any(fun(IncludeFile) -> {ok, #file_info{atime = IncludeTime}} =
		file:read_file_info(IncludeFile), IncludeTime > BinTime end,
		lists:append([
			[filename:join(Path, File) || File <- Files] || {Path, {ok, Files}}
				<- [{Path, file:list_dir(Path)} || Path <- IncludePaths]
		])
	) of true -> c_mod(SrcMod, OutPath, IncludePaths); false -> ok end;

compile_mod(_Error, _, SrcMod, OutPath, IncludePaths) ->
	c_mod(SrcMod, OutPath, IncludePaths).

c_mod(SrcMod, OutPath, IncludePaths) ->
	filelib:ensure_dir(OutPath),
	file:make_dir(OutPath),
	CompileResult = compile:file(SrcMod,
		?CompileOptions(OutPath, IncludePaths)),
	Mod = list_to_atom(filename:basename(SrcMod)),
	code:purge(Mod), code:load_file(Mod),
	io:format(" - ~p~n", [CompileResult]).

copy_app(Path, OutPath) when is_list(Path) ->
	BinPath = filename:join(Path, ?BinDir),
	copy_app(file:list_dir(BinPath), BinPath, OutPath).
copy_app({ok, BinFiles}, BinPath, OutPath) ->
	[_, App|_] = lists:reverse(filename:split(BinPath)),
	io:format("Copying app: ~p~n", [App]),
	AppFiles = [filename:join(BinPath, File) ||
		File <- BinFiles, filename:extension(File) == ?AppExt],
	[copy_app_file(AppFile, OutPath) || AppFile <- AppFiles];
copy_app(Error, _, _) -> Error.

copy_app_file(AppFile, OutPath) ->
	filelib:ensure_dir(OutPath),
	file:make_dir(filename:basename(OutPath)),
	file:copy(AppFile, filename:join(OutPath, filename:basename(AppFile))).

substract(L1, L2) ->
	lists:dropwhile(fun({Key, _}) -> lists:keymember(Key, 1, L2) end, L1).

uniq(L) -> uniq(L, []).
uniq(L = [{Key, _}|_], Acc) -> uniq(lists:keymember(Key, 1, Acc), L, Acc);
uniq([], Acc) -> lists:reverse(Acc).
uniq(true, [_|T], Acc) -> uniq(T, Acc);
uniq(false, [H|T], Acc) -> uniq(T, [H|Acc]);
uniq(_, [], Acc) -> uniq([], Acc).
