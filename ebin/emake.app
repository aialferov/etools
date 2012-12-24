%%%-------------------------------------------------------------------
%%% Created: 24 Dec 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, emake, [
	{id, "emake"},
	{vsn, "0.0.1"},
	{description, "Erlang building tool"},
	{modules, [emake]},
	{registered, []},
	{applications, [kernel, stdlib, sasl]}
]}.
