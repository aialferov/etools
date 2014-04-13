%%%-------------------------------------------------------------------
%%% @author Anton I Alferov casper@sbsn
%%% @copyright (C) 2014, Anton I Alferov
%%%
%%% Created: 24 Mar 2014 by Anton I Alferov casper@sbsn
%%%-------------------------------------------------------------------

-module(etools_template).
-export([make/3]).

-define(SrcDir, "src").

-define(TmplNames(Type), case Type of
	app -> ["app_src", "interface", "application", "supervisor", "gen_server"];
	lib -> ["lib_src", "interface"];
	rel -> ["lib_src", "interface"]
end).

-define(FileNameSuffix(TmplName), case TmplName of
	"application" -> "_app";
	"supervisor" -> "_sup";
	"gen_server" -> "_server";
	_ -> ""
end).

-define(FileExt(TmplName), case TmplName of
	TmplName when TmplName == "app_src"; TmplName == "lib_src" -> ".app.src";
	_ -> ".erl"
end).

-define(FileName(TmplName, Name),
	Name ++ ?FileNameSuffix(TmplName) ++ ?FileExt(TmplName)).

make(Type, Name, Options) ->
	filelib:ensure_dir(filename:join(Name, ?SrcDir) ++ "/"),
	[from_tmpl(TmplName, Name, Options) || TmplName <- ?TmplNames(Type)].

from_tmpl(TmplName, Name, Options) -> file:write_file(
	filename:join([Name, ?SrcDir, ?FileName(TmplName, Name)]),
	etools_tmpl:make(TmplName, [{name, Name}|Options])
).
