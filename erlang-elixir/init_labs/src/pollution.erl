%%%-------------------------------------------------------------------
%%% @author jkaczmarski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2025 15:04
%%%-------------------------------------------------------------------
-module(pollution).
-author("jkaczmarski").

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3]).


% measurement: list[#measurement]
-record(station, {name, coordinates, measurements}).
% stations: list[#station]
-record(monitor, {stations}).

create_monitor() ->
  #monitor{
    stations = []
  }.

add_station(Name, {X, Y}, #monitor{stations = Stations} = M_in) ->
  case lists:any(fun(#station{name = N}) -> N == Name end, Stations) orelse
    lists:any(fun(#station{coordinates = {CX, CY}}) -> {CX, CY} == {X, Y} end, Stations) of
    true ->
      {error, already_exists};
    false ->
      New_station = #station{name = Name, coordinates = {X, Y}, measurements = []},
      New_stations = Stations ++ [New_station],
      M_in#monitor{stations = New_stations}
  end.

add_value(NameOrCoords, Time, Type, Value, #monitor{stations = Stations} = M_in) ->
  case find_station(NameOrCoords, Stations) of
    {ok, Station} ->
      Coords = Station#station.coordinates,
      Measurements = Station#station.measurements,

      case lists:any(
        fun({C, T, Tp, _}) ->
          C == Coords andalso T == Time andalso Tp == Type
        end, Measurements) of
        true ->
          {error, measurement_already_exists};
        false ->
          NewMeasurement = {Coords, Time, Type, Value},
          UpdatedStation = Station#station{
            measurements = Measurements ++ [NewMeasurement]
          },
          UpdatedStations = replace_station(Station, UpdatedStation, Stations),
          M_in#monitor{stations = UpdatedStations}
      end;
    {error, not_found} ->
      {error, station_not_found}
  end.

remove_value(NameOrCoords, Time, Type, #monitor{stations = Stations} = M_in) ->
  case find_station(NameOrCoords, Stations) of
    {ok, Station} ->
      Coords = Station#station.coordinates,
      OldMeasurements = Station#station.measurements,

      case lists:partition(
        fun({C, T, Tp, _}) ->
          C == Coords andalso T == Time andalso Tp == Type
        end, OldMeasurements) of

        {[_ | _], Rest} ->
          UpdatedStation = Station#station{measurements = Rest},
          UpdatedStations = replace_station(Station, UpdatedStation, Stations),
          M_in#monitor{stations = UpdatedStations};

        {[], _} ->
          {error, measurement_not_found}
      end;

    {error, not_found} ->
      {error, station_or_measurement_not_found}
  end.

get_one_value(NameOrCords, Time, Type, #monitor{stations = Stations}) ->
  case find_station(NameOrCords, Stations) of
    {ok, Station} ->
      Coords = Station#station.coordinates,
      Measurements = Station#station.measurements,

      case lists:partition(
        fun({C, T, Tp, _}) ->
          C == Coords andalso T == Time andalso Tp == Type
        end, Measurements) of

        {[{_, _, _, Value} | _], _} ->
          Value;
        {[], _} ->
          {error, measurement_not_found}

      end;
    {error, not_found} ->
      {error, station_not_found}
  end.

get_station_mean(NameOrCoords, Type, #monitor{stations = Stations}) ->
  case find_station(NameOrCoords, Stations) of
    {ok, Station} ->
      Measurements = Station#station.measurements,

      Filtered = [V || {_, _, T, V} <- Measurements, T == Type],

      case Filtered of
        [] ->
          {error, measurement_not_found};
        _ ->
          lists:sum(Filtered) / length(Filtered)
      end;

    {error, not_found} ->
      {error, station_not_found}
  end.

get_daily_mean(Type, Date, #monitor{stations = Stations}) ->
  AllMeasurements = lists:flatten([S#station.measurements || S <- Stations]),

  Filtered = [
    Value || {_, {D, _}, T, Value} <- AllMeasurements,
    D == Date,
    T == Type
  ],

  case Filtered of
    [] ->
      {error, measurement_not_found};
    _ ->
      lists:sum(Filtered) / length(Filtered)
  end.


find_station(Name, Stations) when is_list(Name) ->
  case lists:filter(fun(S) -> S#station.name == Name end, Stations) of
    [Match | _] -> {ok, Match};
    [] -> {error, not_found}
  end;

find_station({X, Y}, Stations) ->
  case lists:filter(fun(S) -> S#station.coordinates == {X, Y} end, Stations) of
    [Match | _] -> {ok, Match};
    [] -> {error, not_found}
  end.

replace_station(Old, New, Stations) ->
  lists:map(
    fun(S) when S#station.name == Old#station.name -> New;
      (S) -> S
    end,
    Stations).