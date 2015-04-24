%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@alferov.me>
%%% @copyright (C) 2015, Anton I Alferov
%%%
%%% Created: 23 Apr 2015 by Anton I Alferov <casper@alferov.me>
%%%-------------------------------------------------------------------

-define(ProjectTemplatesDir, "projects").
-define(FileTemplatesDir, "files").

-define(ProjectTemplate(Dir, Name),
	filename:join([Dir, ?ProjectTemplatesDir, Name])).

-define(FileTemplate(Dir, Name),
	filename:join([Dir, ?FileTemplatesDir, Name])).
