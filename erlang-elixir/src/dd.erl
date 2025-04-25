%%%-------------------------------------------------------------------
%%% @author jkaczmarski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2025 14:13
%%%-------------------------------------------------------------------
-module(dd).
-author("jkaczmarski").

%% API
-export([rand_locs/1, dist/2, find_closest/2, find_for_person/2, find_for_person/3, find_closest_parallel/2]).

rand_locs(N) -> [{rand:uniform(1000), rand:uniform(1000)} || _ <- lists:seq(1, N)].

dist({X1, Y1}, {X2, Y2}) -> math:sqrt((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)).

find_for_person(PeopleLocations, SensorsLocations) ->
  lists:min(
    [{dist(PeopleLocations, SL), {PeopleLocations, SL}} || SL <- SensorsLocations]
  ).

find_closest(PeopleLocations, SensorsLocations) ->
  lists:min(
    [find_for_person(PL, SensorsLocations) || PL <- PeopleLocations]
  ).

find_for_person(PeopleLocations, SensorsLocations, ParentPID) ->
  ParentPID ! find_for_person(PeopleLocations, SensorsLocations).

find_closest_parallel(PeopleLocations, SensorsLocations) ->
  [spawn(?MODULE, find_for_person, [PL, SensorsLocations, self()]) || PL <- PeopleLocations],
  lists:min([receive D -> D end || _ <- PeopleLocations]).