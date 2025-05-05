%%%-------------------------------------------------------------------
%%% @author jkaczmarski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Mar 2025 15:12
%%%-------------------------------------------------------------------
-module(quick).
-author("jkaczmarski").

%% API
-export([less_then/2, grt_eq_than/2, qs/1, random_elems/3, reverse/1, compare_speeds/3]).

less_then(List, Arg) ->
  [X || X <- List, X < Arg].

grt_eq_than(List, Arg) ->
  [X || X <- List, X >= Arg].

qs([]) ->
  [];

qs([Pivot|Tail]) ->
  qs(
    less_then(Tail,Pivot)) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot)
  ).

random_elems(N,Min,Max) ->
  % Adding this one and then subtracting to achieve from Min to Max including both ends
  [rand:uniform(Max - Min + 1) + (Min - 1) || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  {Out_one, _} = timer:tc(Fun1, [List]),
  {Out_two, _} = timer:tc(Fun2, [List]),
  io:format("~w", [abs(Out_two - Out_one)]).


reverse(L) -> reverse(L,[]). % use an accumulator to create a tail recursive function

reverse([],R) -> R;
reverse([H|T],R) -> reverse(T,[H|R]).