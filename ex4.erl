%%%-------------------------------------------------------------------
%%% @author omerlux
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23 April 2019 12:19 PM
%%%-------------------------------------------------------------------
-module(ex4).
-author("omerlux").

%%API
-export([flatten/1, smaller/2, replace/3, mapSub/2]).
%%%-------------------------------------------------------------------
%% flatten - return a flat list
flatten([]) -> [];
flatten(List) -> flat(List,[]). % internal function flat

flat([],NewList) -> NewList; % done
flat([H|T],NewList) when is_list(H) -> flat(T,flat(H,NewList)); % H is a list, flat it and continue
flat([H|T],NewList) -> flat(T,NewList++[H]).      % H is a value, add it to the list.
%%%-------------------------------------------------------------------
%% smaller - returns a list of true/false atoms according to elements that are smaller or equal to thr
smaller(List,Thr) when is_list(List) -> lists:map(fun(X) -> X=<Thr end, List);     % X<=Thr is only true or false,
smaller(_,_) -> listIsntList.
%the lists:map is making a list contains true/false according to the X element in the List
%%%-------------------------------------------------------------------
% replace - replaces all instances of old wth new in list
replace([],_,_) -> [];
replace(List, Old, New) -> swap (List, Old, New, []). % sending to internal function

swap([],_,_,NewList) -> NewList;    % done - return the new list
swap([Old|T], Old, New, NewList) -> swap(T, Old, New, NewList++[New]); % H is Old so change it to New
swap([H|T], Old, New, NewList) -> swap(T, Old, New, NewList++[H]). % H isn't Old so dont change it
%%%-------------------------------------------------------------------
% mapSub - subtracts elements of List1 by Arg2
mapSub([],[]) -> [];            % 3 basic states
mapSub([],_) -> lenError;
mapSub(List1,[]) -> List1;
mapSub(List1, Arg2) when length(List1) =/= length(Arg2) -> lenError;
mapSub(List1, Arg2) when not is_list(List1) or (not is_number(Arg2) and not is_list(Arg2)) -> lenError; % List1 isn't list, Arg2 isn't num/list
mapSub(List1, Arg2) when is_list(List1) and is_number(Arg2) -> 
              case length([X|| X <- List1, is_number(X)])==length(List1) of
                true -> lists:map(fun(X) -> X-Arg2 end, List1);
                false -> notAllElementsNum
              end;
mapSub(List1, Arg2) when is_list(List1) and is_list(Arg2) ->
              case length([X|| X <- List1, is_number(X)])==length(List1) of
                true -> case length([X|| X <- Arg2, is_number(X)])==length(Arg2) of
                          true -> subtup(lists:zip(List1,Arg2));
                          false -> notAllElementsNum
                        end;
                false -> notAllElementsNum
              end.

subtup(List) -> lists:map( fun({X,Y}) -> X-Y end, List). % subtract right element in Arg2, from left element in List1
