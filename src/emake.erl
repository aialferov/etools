%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created: 23 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(emake).
-export([all/0]).

-define(ConfigFile, "Emakefile.em").

-define(AppExt, ".app").
-define(SrcExt, ".erl").

-define(OutDir, "ebin").
-define(BinDir, "ebin").
-define(SrcDir, "src").
-define(IncludeDir, "include").

-define(CompileOptions(OutDir, IncludeDirs),
	[report_errors, report_warnings, {outdir, OutDir}] ++
	[{i, IncludeDir} || IncludeDir <- IncludeDirs]
).

all() ->
	Deps = read_deps(),
	IncludePaths = [Dep ++ "/" ++ ?IncludeDir || Dep <- Deps],
	[build(Dep, ?OutDir, IncludePaths) || Dep <- Deps],
	[copy_app(Dep, ?OutDir) || Dep <- Deps],
	build(".", ?OutDir, IncludePaths ++ [?IncludeDir]),
	ok.

read_deps() -> lists:reverse([Dep || {_ID, Dep} <- lists:foldl(
	fun(Dep = {ID, _}, Deps) -> case lists:keyfind(ID, 1, Deps) of
		{ID, _} -> Deps; false -> [Dep|Deps]
	end end, [], lists:flatten(read_deps("."))
)]).

read_deps(Path) when is_list(Path) ->
	read_deps(file:consult(Path ++ "/" ++ ?ConfigFile));
read_deps({ok, Config}) -> read_deps(lists:keyfind(deps, 1, Config));
read_deps({deps, Deps}) -> 
	[read_deps(Dep) || Dep <- Deps] ++
		[{filename:basename(Dep), Dep} || Dep <- Deps];
read_deps(false) -> []; read_deps({error, enoent}) -> [].

build(Path, OutPath, IncludePaths) ->
	io:format("Building: ~p~n", [Path]),
	SrcPath = Path ++ "/" ++ ?SrcDir,
	{ok, SrcFiles} = file:list_dir(SrcPath),
	SrcMods = [SrcPath ++ "/" ++ filename:rootname(File) ||
		File <- SrcFiles, filename:extension(File) == ?SrcExt],
	[compile_mod(SrcMod, OutPath, IncludePaths) || SrcMod <- SrcMods].

copy_app(Path, OutPath) ->
	io:format("Copying app: ~p~n", [Path]),
	BinPath = Path ++ "/" ++ ?BinDir,
	{ok, BinFiles} = file:list_dir(BinPath),
	AppFiles = [BinPath ++ "/" ++ File ||
		File <- BinFiles, filename:extension(File) == ?AppExt],
	[copy_app_file(AppFile, OutPath) || AppFile <- AppFiles].

copy_app_file(AppFile, OutPath) ->
	file:copy(AppFile, OutPath ++ "/" ++ filename:basename(AppFile)).
	
compile_mod(SrcMod, OutPath, IncludePaths) ->
	CompileResult = compile:file(SrcMod,
		?CompileOptions(OutPath, IncludePaths)),
	Mod = list_to_atom(filename:basename(SrcMod)),
	code:purge(Mod),
	code:load_file(Mod),
	io:format(" - ~p~n", [CompileResult]).
