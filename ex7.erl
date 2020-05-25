%%%-------------------------------------------------------------------
%%% @author Omer
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. May 2020 8:46
%%%-------------------------------------------------------------------
-module(ex7).
-author("OmerLux").

%% API
-export([calc/3, steady/1]).
%%%-------------------------------------------------------------------
%% calc -> returns  A:B
calc(division,A,B)->
  try A/B of
    _ -> A/B
  catch       % protected
    Type:Pattern -> {warning, Type, Pattern}
  end.

%% steady(F) -> evaluates function F and logs all its exceptions
steady(F) ->
  try F() of
    Success -> appendIt({erlang:system_time(), success, Success})
  catch
    error:Error -> appendIt({erlang:system_time(), error, Error});
    exit:Exit -> appendIt({erlang:system_time(), exit, Exit});
    throw:Throw -> appendIt({erlang:system_time(), throw, Throw})
  end.

appendIt(Data) ->
  Filename = "myLog_205500390.elog",
  DataString = lists:flatten(io_lib:format("~p~n", [Data])),
  case file:read_file_info(Filename) of
      {error, enoent} ->
        file:write_file(Filename, DataString);  % create the file and write
      {ok, _FileInfo} ->
        file:write_file(Filename, DataString,[append]) % append data to existing file
  end.
