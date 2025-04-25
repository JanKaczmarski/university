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

-export([newReading/2, number_of_readings_wrapper/2, number_of_readings/2, calculate_max/2, calculate_max_wrapper/2, calculate_mean/2, calculate_sum_and_count/2]).

newReading(City, Measurement) ->
  {
    pomiar,
    City,
    {date(), time()},
    Measurement
  }.

% Number of readings
number_of_readings_wrapper(Readings, Date) ->
  number_of_readings(Readings, Date).

number_of_readings([{pomiar, _, Date, _} | Tail], Date) ->
  1 + number_of_readings(Tail, Date);

number_of_readings(_, _) ->
  0.

% Calculate max
calculate_max_wrapper(Readings, Type) ->
  calculate_max(Readings, Type).

calculate_max([], _) ->
  0;

calculate_max([{pomiar, _, _, {Type, Value}} | Tail], Type) ->
  Tmp = calculate_max(Tail, Type),
  max(Value, Tmp);

calculate_max([_ | Tail], Type) ->
  calculate_max(Tail, Type).

calculate_mean(Readings, Type) ->
  % Prevent division by zero
  case calculate_sum_and_count(Readings, Type) of
    {0, 0} -> 0;
    {Sum, Count} -> Sum / Count
  end.

calculate_sum_and_count([], _) ->
  {0, 0};

calculate_sum_and_count([{pomiar, _, _, {Type, Value}} | Tail], Type) ->
  {Sum, Count} = calculate_sum_and_count(Tail, Type),
  {Value + Sum, Count + 1};

calculate_sum_and_count([_ | Tail], Type) ->
  calculate_sum_and_count(Tail, Type).

% Commands to run as part of presentation
% List = [{'London', {'PM10', 20}}, {'Seattle', {'PM10', 30}}, {'New York', {'DM30', 150}}, {'Warsaw', {'PM10', 40}}, {'London', {'PM10', 20}}].
% Readings = lists:map(fun({C, M}) -> main:newReading(C, M) end, List).
% main:number_of_readings_wrapper(Readings, <HERE_SHOULD_GO_DATETIME_FROM_ABOVE_READING!!>).
% main:calculate_max(Readings, 'PM10').
% main:calculate_mean(Readings, 'PM10').


%%Struct = {
%%% It's said that atoms don't apply to garbage collection. Think about changing this pomiar to String
%%pomiar,
%%'London',
%%% YYYY, MM, DD, hh, mm, ss
%%{date(), time()},
%%[
%%% RodzajCzytnika, wartosc
%%{'PM10', 123},
%%{'PM2.5', 50}
%%]
%%}.
