%%%-------------------------------------------------------------------
%%% @author jkaczmarski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2025 16:11
%%%-------------------------------------------------------------------
-module(myLists).
-author("jkaczmarski").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1, power/2]).

contains([], _) ->
  false;
contains([H | _T], H) ->
  true;
contains([_ | T], E) -> contains(T, E).

duplicateElements([]) ->
  [];
duplicateElements([H | T]) ->
  [H, H] ++ duplicateElements(T).

sumFloats(L) when is_list(L) ->
  sumFloats(L, 0).
sumFloats([], Acc) ->
  Acc;
sumFloats([H | T], Acc) when is_float(H) ->
  sumFloats(T, Acc + H);
sumFloats([_ | T], Acc) ->
  sumFloats(T, Acc).

power(X, 0) when is_number(X) ->
  1;
power(X, K) ->
  X * power(X, K - 1).
