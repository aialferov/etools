%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 23 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(emake).
-export([all/0, clean/0, deepclean/0]).

-define(ConfigFile, "Emakefile.em").

-define(AppExt, ".app").
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
	DepPaths = read_deps(),
	IncludePaths = [filename:join(
		DepPath, ?IncludeDir) || DepPath <- DepPaths],
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

read_deps() -> lists:reverse([Dep || {_ID, Dep} <- lists:foldl(
	fun(Dep = {ID, _}, Deps) -> case lists:keyfind(ID, 1, Deps) of
		{ID, _} -> Deps; false -> [Dep|Deps]
	end end, [], lists:flatten(read_deps("."))
)]).

read_deps(Path) ->
	read_deps(file:consult(filename:join(Path, ?ConfigFile)), Path).
read_deps({ok, Config}, Path) ->
	read_deps(lists:keyfind(deps, 1, Config), Path);
read_deps({deps, Deps}, Path) ->
	[read_deps(dep_path(Dep, Path)) || Dep <- Deps] ++
		[{dep_id(Dep), dep_path(Dep, Path)} || Dep <- Deps];
read_deps(false, _) -> [];
read_deps({error, enoent}, _) -> [].

dep_id({DepPath, _Options}) -> filename:basename(DepPath).

dep_path({DepPath, Options}, Path) ->
	case lists:keyfind(git, 1, Options) of
		{git, DepGit} -> load_dep(filename:absname(DepPath, Path), DepGit);
		false -> filename:absname(DepPath, Path)
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
