%%%-------------------------------------------------------------------
%%% @author Omer
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. May 2020 8:25
%%%-------------------------------------------------------------------
-module(ex6).
-author("OmerLux").

%% API
-export([songGen/3,songList/1]).
%%%-------------------------------------------------------------------
% songList(Songs) - return a graph of the songs
songList(Songs) when not is_list(Songs) -> io:format("Bad arguments~n");         % error
songList([]) ->
  Graph = digraph:new(),
  io:format("The graph has 0 edges.~n"),
  Graph;
songList(Songs) ->
  VerGraph = addVertices (digraph:new(), Songs),                                              % adding vertices to the graph
  Graph = connectVertices (VerGraph, digraph:vertices(VerGraph), digraph:vertices(VerGraph)), % connecting each V to another
  io:format("The graph has ~p edges.~n",[digraph:no_edges(Graph)]),
  Graph.


% addVertices(Graph,List) - adding all vertices to the graph
addVertices(Graph, []) -> Graph;                                                       % no more songs to add
addVertices(Graph, [V|T]) ->
  digraph:add_vertex(Graph,V),                                                         % added the vertex
  addVertices(Graph, T).

% connectVertices( Graph, V1s, V2s) - connecting V1 to V2
connectVertices(Graph, [], _) -> Graph;
connectVertices(Graph,[V1|T1], [V2|T2]) ->
  % sending V1 neighbors to connect with V1 - [V2|T2] will remain whole the vertices - V1=V2 always
  Graph_V1_connected = addV1edges(Graph, V1, connections([V2|T2])), % connections will find neighbors for V1=V2
  % and again, continue with T1 the same process
  connectVertices(Graph_V1_connected, T1, T2++[V2]).                % omitting V1 - will add neighbors to the others, while T1=T2

% connections([H|T]) - returns a list to connect to H - by first char is the last of H
connections([]) -> [];
connections([H|T]) -> [X || X<-T,       % compare last char to first of the others, and takes them as neighbors
  string:sub_string(H, string:length(H), string:length(H)) == string:sub_string(X, 1, 1)].

% addV1edges(Graph, V1, V1neighbors) - connect V1 with it's neighbors
addV1edges(Graph,_,[]) -> Graph;
addV1edges(Graph,V1,[H|T]) ->
  digraph:add_edge(Graph,V1,H),         % adding edge
  addV1edges(Graph,V1,T).

%% songGen(G,Start,End) - returns the shortest song list that start with the song Start and ends with End
songGen(Graph,Start,End) -> digraph:get_short_path(Graph,Start,End).