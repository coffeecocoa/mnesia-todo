-module(db_server).
-behaviour (gen_server).
-export([start_link/0,install/0,get/1,
	add/2,get/0,update/3,delete/1,search/1]).

-export([init/1,handle_call/3,handle_cast/2,
	handle_info/2,terminate/2,code_change/3]).

-include("todo_record.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record (state, {}).

install() ->
	Node = [node()|nodes()],
	mnesia:create_schema(Node),
	%% application:start(mnesia),
	rpc:multicall(Node,application,start,[mnesia]),
	mnesia:create_table(todo,[{attributes,record_info(fields,todo)},
								{index,[#todo.content]},
								{disc_copies,Node},
								{type,ordered_set}]),
	%% application:stop(mnesia),
	rpc:multicall(Node,application,stop,[mnesia]),
	ok.
%%========================== Client APi ==================================

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

add(Content,Status) ->
	gen_server:call(?MODULE,{add,Content,Status}).

get() ->
	gen_server:call(?MODULE,get).

get(ID) ->
	gen_server:call(?MODULE,{get,ID}).

update(Id,Content,Status) ->
	gen_server:call(?MODULE,{update,Id,Content,Status}).

delete(Id) ->
	gen_server:call(?MODULE,{delete,Id}).

search(Content) ->
	gen_server:call(?MODULE,{search,Content}).

%%========================== Server API ===================================
init([]) ->
	process_flag(trap_exit,true),
	{ok,#state{}}.

handle_call({add,Content,Status},_From,State) ->

	{ok,Todo} = create_todo(Content,Status),
	
	{reply,Todo,State};

handle_call({get,ID},_From,State) ->
	{ok,Todo} = get_todo(ID),
	
	{reply,Todo,State};

handle_call(get,_From,State) ->
	{ok,Todo} = get_list(),
	{reply,Todo,State};

handle_call({update,Id,Content,Status},_From,State) ->
	{ok,Todo} = get_todo(Id),
	{ok,Result} = update_todo(Todo#todo.id, Content,Status),
	{reply,Result,State};

handle_call({delete,Id},_From,State) ->
	{ok, Todo} = get_todo(Id),
	{atomic,Result} = mnesia:transaction(fun() ->
		mnesia:delete_object(Todo)
	end),
	{reply,Result,State};

handle_call({search,Content},_From,State) ->
	{ok,Todo} = search_todo(Content),
	{reply,Todo,State}.


handle_cast(_Msg,State) -> {noreply,State}.
handle_info(_Info,State) -> {noreply,State}.

terminate(_Reason,_State) ->
	io:format("~p stopping~n",[?MODULE]),
	ok.

code_change(_OldVsn,State,_Extra) -> {ok,State}.

%%=========================== Private Functions =================================

get_list() ->
	Query = fun() ->
		qlc:e(
			qlc:q([X || X <- mnesia:table(todo)])
		)
	end,
	{atomic,Todo} = mnesia:transaction(Query),
	{ok,Todo}.

get_todo(ID) ->
	{atomic,Todo} = mnesia:transaction(fun() ->
			case mnesia:read(todo,ID,read) of
				[Item] -> Item;
				[] -> []
			end
	end),
	{ok,Todo}.

search_todo(Content) ->
	Query = fun() ->
		Q = qlc:q([E || E <- mnesia:table(todo), E#todo.content == Content]),
		qlc:e(Q)
	end,
	{atomic,Todo} = mnesia:transaction(Query),
	{ok,Todo}.

create_todo(Content,Status) ->
	Time = erlang:system_time(seconds),
	
	Id = erlang:system_time(nano_seconds),
	{atomic,Todo} = mnesia:transaction(fun() ->
		New  = #todo{
			id = Id,
			content = Content,
			status = Status,
			created = Time
			},
		mnesia:write(New),
		New
	end),
	{ok,Todo}.

update_todo(Id,Content,Status) ->
	Time = erlang:system_time(seconds),

	Query  = fun() ->
		Update = #todo{
			id = Id,
			content = Content,
			status = Status,
			created = Time
		},
		mnesia:write(Update),
		Update
	end,
	{atomic, Todo} = mnesia:transaction(Query),
	{ok,Todo}.

