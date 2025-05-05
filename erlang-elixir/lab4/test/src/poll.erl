%%%-------------------------------------------------------------------
%%% @author jkaczmarski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(poll).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(poll_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #poll_state{}}.

handle_call(_Request, _From, State = #poll_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #poll_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #poll_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #poll_state{}) ->
  ok.

code_change(_OldVsn, State = #poll_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
