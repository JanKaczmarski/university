%%%-------------------------------------------------------------------
%%% @author jkaczmarski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2025 15:31
%%%-------------------------------------------------------------------
-module(main).
-author("jkaczmarski").

%% API
-export([power/2]).

power(X, 0) when is_number(X) ->
  1;
power(X, K) ->
   X * power(X, K - 1).
