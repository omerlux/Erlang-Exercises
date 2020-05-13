%%%-------------------------------------------------------------------
%%% @author omerlux
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Mar 2019 12:19 PM
%%%-------------------------------------------------------------------
-module(ex2).
-author("omerlux").

%%API
-export([findKelem/2, reverse/1, deleteKelem/2, addKelem/3, union/2]).

%% Find the Kth element in a list
findKelem([],_)    -> notFound;                                      % the list is empty
findKelem([H|T],K) -> kElem ([H|T],K).                               % isn't empty - send to recursive function

kElem([],_)    -> notFound;                                          % the list is shorter than K - not such element
kElem([H|_],1) -> H;                                                 % H is the Kth element
kElem([_|T],K) -> kElem(T,K-1).                                      % cutting the head for K times

%% Reverse list items
reverse([])    -> [];                                                % the list is empty
reverse([H])   -> [H];                                               % only 1 item is in the list
reverse([H|T]) -> reverseIt([H|T],[]).                               % reverseIt is the recursive function

reverseIt([],NewList)    -> NewList;                                 % finished
reverseIt([H|T],NewList) -> reverseIt(T,[H|NewList]).                % cutting the head and adding it to the new list head

%% Delete the all instances of Elem
deleteKelem([],_)       -> [];                                       % no element to delete
deleteKelem([H|T],Elem) -> [Var || Var <-[H|T], Var=/=Elem].         % all variables will stay only if var=/=element

%% Add element to the list in the k'th place
addKelem([],1,Elem)     -> [Elem];                                   % when the list is empty
addKelem([H|T],1,Elem)  -> [Elem,H|T];                               % when the place is 1
addKelem([H|T],K,Elem)  -> addIt([],[H|T],K,Elem).                   % addIt is the recursive function

addIt(NewList,[],0,_)        -> reverse(NewList);                    % inverse the list (connection is done through the head)
addIt(NewList,[H|T],0,_)     -> addIt([H|NewList],T,0,0);            % connecting the rest to the list
addIt(NewList,OldList,1,Elem)-> addIt([Elem|NewList],OldList,0,Elem);% connect Elem to the heads of the list
addIt(NewList,[H|T],K,Elem)  -> addIt([H|NewList],T,K-1,Elem).       % searching the K'th

%% Union - union of 2 lists and remove multiple instances
union([],[])  -> [];
union(L1,L2) -> un([],L1,L2).

un(NewList,[],[]) -> reverse(NewList);                               % inverse the list (connection is done through the head)
un(NewList, [],[H2|T2]) -> un([H2|NewList],
                              [],
                              deleteKelem(T2,H2));                   % deleting all H2 elemnts from the second list
un(NewList,[H1|T1],L2) -> un([H1|NewList],
                              deleteKelem(T1,H1),
                              deleteKelem(L2,H1)).                   % deleting all H1 elements from the 2 lists