%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 23 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(emake).
-export([all/0, clean/0]).

-define(ConfigFile, "Emakefile.em").

-define(AppExt, ".app").
-define(SrcExt, ".erl").

-define(DepsDir, "deps").
-define(BinDir, "ebin").
-define(SrcDir, "src").
-define(IncludeDir, "include").

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
	build(".", ?BinDir, IncludePaths ++ [?IncludeDir]),
	ok.

clean() ->
	os:cmd("rm -r " ++ ?DepsDir),
	os:cmd("rm -f" ++ ?BinDir ++ "/*.beam"),
	ok.

read_deps() -> lists:reverse([Dep || {_ID, Dep} <- lists:foldl(
	fun(Dep = {ID, _}, Deps) -> case lists:keyfind(ID, 1, Deps) of
		{ID, _} -> Deps; false -> [Dep|Deps]
	end end, [], lists:flatten(read_deps("."))
)]).

read_deps(Path) when is_list(Path) ->
	read_deps(file:consult(filename:join(Path, ?ConfigFile)));
read_deps({ok, Config}) -> read_deps(lists:keyfind(deps, 1, Config));
read_deps({deps, Deps}) -> 
	[read_deps(Dep) || Dep <- Deps] ++
		[{filename:basename(Dep), Dep} || Dep <- Deps];
read_deps(false) -> []; read_deps({error, enoent}) -> [].

build(Path, OutPath, IncludePaths) ->
	io:format("Building: ~p~n", [Path]),
	SrcPath = filename:join(Path, ?SrcDir),
	{ok, SrcFiles} = file:list_dir(SrcPath),
	SrcMods = [filename:join(SrcPath, filename:rootname(File)) ||
		File <- SrcFiles, filename:extension(File) == ?SrcExt],
	[compile_mod(SrcMod, OutPath, IncludePaths) || SrcMod <- SrcMods].

compile_mod(SrcMod, OutPath, IncludePaths) ->
	filelib:ensure_dir(OutPath),
	file:make_dir(OutPath),
	CompileResult = compile:file(SrcMod,
		?CompileOptions(OutPath, IncludePaths)),
	Mod = list_to_atom(filename:basename(SrcMod)),
	code:purge(Mod), code:load_file(Mod),
	io:format(" - ~p~n", [CompileResult]).

copy_app(Path, OutPath) ->
	io:format("Copying app: ~p~n", [Path]),
	BinPath = filename:join(Path, ?BinDir),
	{ok, BinFiles} = file:list_dir(BinPath),
	AppFiles = [filename:join(BinPath, File) ||
		File <- BinFiles, filename:extension(File) == ?AppExt],
	[copy_app_file(AppFile, OutPath) || AppFile <- AppFiles].

copy_app_file(AppFile, OutPath) ->
	filelib:ensure_dir(OutPath),
	file:make_dir(filename:basename(OutPath)),
	file:copy(AppFile, filename:join(OutPath, filename:basename(AppFile))).
