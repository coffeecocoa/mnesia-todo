-module(db_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-include("todo_record.hrl").

start(normal, _Args) ->
	mnesia:wait_for_tables([todo],5000),
	db_sup:start_link().

stop(_State) ->
	ok.
