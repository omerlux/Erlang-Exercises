%%%-------------------------------------------------------------------
%%% @author Omer
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2020 9:21
%%%-------------------------------------------------------------------
-module(ex5).
-author("omerlux").

%% API
-export([ring_parallel/2, ring_serial/2,mesh_parallel/3,mesh_serial/3]).
%%%-------------------------------------------------------------------
%% ring_parallel(N,M) creates a ring of N process with M messages
%% !!! DONE IT FROM THE OLD PATTERN WHEN THE PROGRAM IS SEQUENTIAL !!!
ring_parallel(N,_) when N=<0 -> io:format("Bad arguments~n");         % error
ring_parallel(_,M) when M=<0 -> io:format("Bad arguments~n");         % error
ring_parallel(N,M) ->
  T1 = os:timestamp(),                                                % make the initial timestamp
  Root = self(),                                                      % saving the root process
  register(pid1, spawn(fun() -> receiveAction(Root, N, M, T1) end)),  % pid1 will create other pids
  pid1 ! {"create", 2},
  receive                                                             % waiting for last call
    Ans -> Ans
  end.

% prcsToPid - returns the pid#I atom
prcsToPid(I) -> list_to_atom("pid" ++ integer_to_list(I)).

% RecieveAction (N,M,T1) - this is the receive function. N = # processes, M = # messages, T1 = init time
receiveAction(Root, N, M, T1) ->
  receive
    {"create", NextPid} when NextPid == N+1 ->     % Last creation of pid
      pid1 ! {1, "Send", 0},
      receiveAction(Root,N,M,T1);

    {"create", NextPid} ->
      register(prcsToPid(NextPid), spawn(fun() -> receiveAction(Root, N, M, T1) end)),  % create next PID
      NextIndex = NextPid+1,
      prcsToPid(NextPid) ! {"create",NextIndex},                         % send msg to Next
      receiveAction(Root,N,M,T1);

  % actins for pid1 only. if you got a message, than send the next one. (until M)
    {1, "Send", M} ->
      %io:fwrite("pid~p got the ~p message, and last. Bye!~n", [1,M]),   % TODO: Comment
      Root ! {round(timer:now_diff(os:timestamp(), T1) / math:pow(10,3)), M, M};
    {1, "Send", Mindex} ->                                               % 1 is for pid1 - send message Mindex
      %io:fwrite("pid~p got the ~p message~n", [1,Mindex]),              % TODO: Comment
      pid2 ! {2, "Send", Mindex+1},                                      % send pid2 a new message
      receiveAction(Root,N,M,T1);

  % for each prcs that isn't pid1 send the message to next pid
    {Index, "Send", M} ->                                                % received message that was sent - last one - exit after
      %io:fwrite("pid~p got the ~p message. Bye!~n", [Index,M]),         % TODO: Comment
      prcsToPid(max((Index+1) rem (N+1), 1)) ! {max((Index+1) rem (N+1), 1), "Send", M};  % send the next pid or to pid1
    {Index, "Send", Mindex} ->                                           % received message that was sent
      %io:fwrite("pid~p got the ~p message~n", [Index,Mindex]),          % TODO: Comment
      prcsToPid(max((Index+1) rem (N+1), 1)) ! {max((Index+1) rem (N+1), 1), "Send", Mindex},  % send the next pid or to pid1
      receiveAction(Root,N,M,T1);

    Error -> io:format("Bad arguments: ~p~n", [Error])
  end.


%% ring_serial(V,M) create a circle of V vertices. all the vetices are managed by the main process.
ring_serial(V,_) when V=<0 -> io:format("Bad arguments~n");         % error
ring_serial(_,M) when M=<0 -> io:format("Bad arguments~n");         % error
ring_serial(V,M) ->
  T1 = os:timestamp(),
  Root = self(),
  register(main, spawn(fun() -> receiveSelf(Root,V,M,T1) end)),         % creating single process
  main ! {1, "Send", 0},
  receive
    Ans -> Ans
  end.

% recieveSelf (N,M,T1) - this is the receive function. N = # processes, M = # messages, T1 = init time
% the process will send itself messages
receiveSelf(Root, N, M, T1) ->
  receive
  % actins for v1 only. if you got a message, than send the next one. (until M)
    {1, "Send", M} ->
      %io:fwrite("v~p got the ~p message, and last. Bye!~n", [1,M]),     % TODO: Comment
      Root ! {round(timer:now_diff(os:timestamp(), T1) / math:pow(10,3)), M, M};% sending the root process the answer
    {1, "Send", Mindex} ->                                               % 1 is for pid1 - send message Mindex
      %io:fwrite("v~p got the ~p message~n", [1,Mindex]),                % TODO: Comment
      main ! {2, "Send", Mindex+1},                                      % send pid2 a new message
      receiveSelf(Root, N,M,T1);

  % for each v that isn't v1 send the message to next v
    {Index, "Send", M} ->                                                % received message that was sent - last one - exit after
      %io:fwrite("v~p got the ~p message. Bye!~n", [Index,M]),           % TODO: Comment
      main ! {max((Index+1) rem (N+1), 1), "Send", M},  % send the next v or to v1
      receiveSelf(Root, N,M,T1);                                         % DO NOT STOP THE PROCESS. THIS IS THE ONLY 1 WE HAVE
    {Index, "Send", Mindex} ->                                           % received message that was sent
      %io:fwrite("v~p got the ~p message~n", [Index,Mindex]),            % TODO: Comment
      main ! {max((Index+1) rem (N+1), 1), "Send", Mindex},              % send the next v or to v1
      receiveSelf(Root, N,M,T1);

    Error -> io:format("Bad arguments: ~p~n", [Error])
  end.

%-----------------------------------------------------------------------------------------------------------------------

%%% TODO: use monitor which tracks what process are alive. waits until all of them are finished (by ACK msg)
%%% TODO: use atoms Pi Pj which will be the current place of the process.
%%% TODO: hold for each PLACE a map with messages that it received
%% mesh_parallel(N,M,C) -> will create a grid of NxN and choose C which is a place of a tile, and send from it M msgs
mesh_parallel(N,_,C) when C<1 orelse C>N*N -> io:format("Bad arguments~n");         % error
mesh_parallel(N,_,_) when N<1 -> io:format("Bad arguments~n");                      % error
mesh_parallel(_,M,_) when M<1 -> io:format("Bad arguments~n");                      % error
mesh_parallel(N,M,C) ->
  T1 = os:timestamp(),                                                              % make the initial timestamp
  Root = self(),
  MainDict_1 = dict:new(),                                                          % creating dictionary
  MainDict = create_maindict (N,1,1, MainDict_1),                                   % -"-
  {Ci,Cj} = getRowCol(C,N),                                                         % returns the place of the Center
  MessageBoxCounter = create_msg_box(M,maps:new()),                                 % map to record the msg index counter
  register (monitor, spawn(fun() -> monitor_receive(MessageBoxCounter,N,M,Root,{Ci,Cj},true) end)),   % creating the monitor process
  create_all_prcs(1,1,N, MainDict, M, {Ci,Cj},T1),                                  % creating all tile processes
  tileToPid(Ci,Cj) ! {start},                                                       % sending the Center a start message
  receive
    Ans ->
      Ans                                                                           % wait till the end
  end,
  Time = round(timer:now_diff(os:timestamp(), T1) / math:pow(10,3)),
  {Time,M,M*(N*N-1)}.                                                               % if we got here, this will always be the same.

% create_maindict - creates the main dictionary NxN
% dictionary will contain the tile index and a map of messages the tile got
create_maindict(N,N,N, MainDict) ->
  MessageMap = maps:new(),                                                          % this is the message map
  dict:append({N,N}, MessageMap, MainDict);                                         % dict add (tile, message map)
create_maindict(N,Row,N, MainDict) ->
  MessageMap = maps:new(),                                                          % this is the message map
  MainDict2 = dict:append({Row,N}, MessageMap, MainDict),                           % dict add (tile, message map)
  create_maindict(N,Row+1,1, MainDict2);
create_maindict(N,Row,Column, MainDict) ->
  MessageMap = maps:new(),                                                          % this is the message map
  MainDict2 = dict:append({Row,Column}, MessageMap, MainDict),                      % dict add (tile, message map)
  create_maindict(N,Row,Column+1, MainDict2).

% create_msg_box - creates the message box counter for the monitor process
create_msg_box(0,MessageBoxCounter) ->
  MessageBoxCounter;
create_msg_box(M,MessageBoxCounter) ->
  MessageBoxCounter2 = maps:put(M, 0, MessageBoxCounter),                                                % index added - counter=0
  create_msg_box(M-1,MessageBoxCounter2).

% create_all_prcs - creates all the N^2 processes for the tiles, includes the Center
create_all_prcs(N,N,N, MainDict, M, {Ci,Cj},T1) ->
  register(tileToPid(N,N), spawn(fun() -> receiveMesh(MainDict, N, M, {Ci,Cj},T1, true) end)); % true is for parallel
create_all_prcs(I,N,N, MainDict, M, {Ci,Cj},T1) ->
  register(tileToPid(I,N), spawn(fun() -> receiveMesh(MainDict, N, M, {Ci,Cj},T1, true) end)),
  create_all_prcs(I+1,1,N, MainDict, M, {Ci,Cj},T1);
create_all_prcs(I,J,N, MainDict, M, {Ci,Cj},T1) ->
  register(tileToPid(I,J), spawn(fun() -> receiveMesh(MainDict, N, M, {Ci,Cj},T1, true) end)),
  create_all_prcs(I,J+1,N, MainDict, M, {Ci,Cj},T1).

% terminate_all - killing all the N^2 processes for the tiles, includes the Center
terminate_all(N,N,N,{Ci,Cj}) ->
  if ((Ci==N) and (Cj==N))->
    void;
    true -> tileToPid(N,N) ! {die,{N,N}}
  end;
terminate_all(I,N,N,{Ci,Cj}) ->
  if ((Ci==I) and (Cj==N)) ->
    void;
    true -> tileToPid(I,N) ! {die,{I,N}}
  end,
  terminate_all(I+1,1,N,{Ci,Cj});
terminate_all(I,J,N,{Ci,Cj}) ->
  if ((Ci==I) and (Cj==J)) ->
    void;
    true -> tileToPid(I,J) ! {die,{I,J}}
  end,
  terminate_all(I,J+1,N,{Ci,Cj}).

% got_all_received - will return true if all the M messages received N times
got_all_received(_,_,0) -> true;                                                    % got all M messages N times - return true
got_all_received(MsgBoxCounter,Nsqrm1,M) ->
  case maps:get(M,MsgBoxCounter) of
    Nsqrm1 -> got_all_received(MsgBoxCounter,Nsqrm1,M-1);                           % all N^2-1 messages given for the Mth message
    _ -> false                                                                      % not all of them given - return false
  end.

% prcsToPid - returns the pid#I atom
tileToPid(_i,_j) -> list_to_atom("pid_" ++ integer_to_list(_i) ++ "_" ++ integer_to_list(_j)).

% getRowCol - returns the Row and Col of P
getRowCol(P, N) ->
  {ceil(P/N),P-N*floor((P-1)/N)}.

% sendMesh - sends M messages to the mesh - THIS IS FOR C ONLY
sendMeshParallel(N,M,{Ci,Cj}) ->
  sendMmsgParallel(N,M,1,{Ci,Cj}).
sendMmsgParallel(N,M,M,{Row,Col}) ->
  % define the 4 pids around
  PidRight = tileToPid(Row,Col+1), PidLeft = tileToPid(Row,Col-1),
  PidUp = tileToPid(Row-1,Col), PidDown = tileToPid(Row+1,Col),
  % send them a messege { From, Receiver, Target, Num of messege}
  if ((Row+1>=1) and (Row+1=<N) and (Col>=1) and (Col=<N)) ->
    PidDown ! {{Row,Col}, {Row+1,Col}, {to,all}, M};
    true -> void
  end,
  if ((Row>=1) and (Row=<N) and (Col-1>=1) and (Col-1=<N)) ->
    PidLeft ! {{Row,Col}, {Row,Col-1}, {to,all}, M};
    true -> void
  end,
  if ((Row>=1) and (Row=<N) and (Col+1>=1) and (Col+1=<N)) ->
    PidRight ! {{Row,Col}, {Row,Col+1}, {to,all}, M};
    true -> void
  end,
  if ((Row-1>=1) and (Row-1=<N) and (Col>=1) and (Col=<N)) ->
    PidUp ! {{Row,Col}, {Row-1,Col}, {to,all}, M};
    true -> void
  end;
sendMmsgParallel(N,M,Mindex,{Row,Col}) ->
  % define the 4 pids around
  PidRight = tileToPid(Row,Col+1), PidLeft = tileToPid(Row,Col-1),
  PidUp = tileToPid(Row-1,Col), PidDown = tileToPid(Row+1,Col),
  % send them a messege { From, Receiver, Target, Num of messege}
  if ((Row+1>=1) and (Row+1=<N) and (Col>=1) and (Col=<N)) ->
    PidDown ! {{Row,Col}, {Row+1,Col}, {to,all}, Mindex};
    true -> void
  end,
  if ((Row>=1) and (Row=<N) and (Col-1>=1) and (Col-1=<N)) ->
    PidLeft ! {{Row,Col}, {Row,Col-1}, {to,all}, Mindex};
    true -> void
  end,
  if ((Row>=1) and (Row=<N) and (Col+1>=1) and (Col+1=<N)) ->
    PidRight ! {{Row,Col}, {Row,Col+1}, {to,all}, Mindex};
    true -> void
  end,
  if ((Row-1>=1) and (Row-1=<N) and (Col>=1) and (Col=<N)) ->
    PidUp ! {{Row,Col}, {Row-1,Col}, {to,all}, Mindex};
    true -> void
  end,
  sendMmsgParallel(N,M,Mindex+1,{Row,Col}).

% sendMesh - sends M messeges to the mesh - THIS IS FOR C ONLY - for self process
sendMeshSerial(N,M,{Ci,Cj}) ->
  sendMeshSerial(N,M,1,{Ci,Cj}).
sendMeshSerial(N,M,M,{Row,Col}) ->
  % send them a messege { From, Receiver, Target, Num of messege}
  if ((Row+1>=1) and (Row+1=<N) and (Col>=1) and (Col=<N)) ->
    erlang:self() ! {{Row,Col}, {Row+1,Col}, {to,all}, M};
    true -> void
  end,
  if ((Row>=1) and (Row=<N) and (Col-1>=1) and (Col-1=<N)) ->
    erlang:self() ! {{Row,Col}, {Row,Col-1}, {to,all}, M};
    true -> void
  end,
  if ((Row>=1) and (Row=<N) and (Col+1>=1) and (Col+1=<N)) ->
    erlang:self() ! {{Row,Col}, {Row,Col+1}, {to,all}, M};
    true -> void
  end,
  if ((Row-1>=1) and (Row-1=<N) and (Col>=1) and (Col=<N)) ->
    erlang:self() ! {{Row,Col}, {Row-1,Col}, {to,all}, M};
    true -> void
  end;
sendMeshSerial(N,M,Mindex,{Row,Col}) ->
  % send them a messege { From, Receiver, Target, Num of messege}
  if ((Row+1>=1) and (Row+1=<N) and (Col>=1) and (Col=<N)) ->
    erlang:self() ! {{Row,Col}, {Row+1,Col}, {to,all}, Mindex};
    true -> void
  end,
  if ((Row>=1) and (Row=<N) and (Col-1>=1) and (Col-1=<N)) ->
    erlang:self() ! {{Row,Col}, {Row,Col-1}, {to,all}, Mindex};
    true -> void
  end,
  if ((Row>=1) and (Row=<N) and (Col+1>=1) and (Col+1=<N)) ->
    erlang:self() ! {{Row,Col}, {Row,Col+1}, {to,all}, Mindex};
    true -> void
  end,
  if ((Row-1>=1) and (Row-1=<N) and (Col>=1) and (Col=<N)) ->
    erlang:self() ! {{Row,Col}, {Row-1,Col}, {to,all}, Mindex};
    true -> void
  end,
  sendMeshSerial(N,M,Mindex+1,{Row,Col}).

% sendNeighborsParallel - sending neighbors a new message targeted to to the Center - sending to other pids
sendNeighborsParallel({{_,_}, {Toi,Toj}, {Ci,Cj},Mindex}) ->
  % define the 4 pids around
  PidRight = tileToPid(Toi,Toj+1), PidLeft = tileToPid(Toi,Toj-1),
  PidUp = tileToPid(Toi-1,Toj), PidDown = tileToPid(Toi+1,Toj),
  % send them a message { From, Receiver, Target, Num of message}

  try PidDown ! {{Toi,Toj}, {Toi+1,Toj}, {Ci,Cj}, Mindex} of
    _ -> ok
  catch
    _->valid; _:_->valid
  end,
  try PidLeft ! {{Toi,Toj}, {Toi,Toj-1}, {Ci,Cj}, Mindex} of
    _ -> ok
  catch
  _->valid; _:_->valid
  end,
  try PidRight ! {{Toi,Toj}, {Toi,Toj+1}, {Ci,Cj}, Mindex} of
    _ -> ok
  catch
    _->valid; _:_->valid
  end,
  try PidUp ! {{Toi,Toj}, {Toi-1,Toj}, {Ci,Cj}, Mindex} of
    _ -> ok
  catch
    _->valid; _:_->valid
  end.

% sendNeighborsSerial - sending neighbors a new message targeted to to the Center - sending to SELF
sendNeighborsSerial(N,{{_,_}, {Toi,Toj}, {Ci,Cj},Mindex}) ->
  % send them a messege { From, Receiver, Target, Num of message}
  if ((Toi+1>=1) and (Toi+1=<N) and (Toj>=1) and (Toj=<N)) ->
    erlang:self() ! {{Toi,Toj}, {Toi+1,Toj}, {Ci,Cj}, Mindex};
    true -> void
  end,
  if ((Toi>=1) and (Toi=<N) and (Toj-1>=1) and (Toj-1=<N)) ->
    erlang:self() ! {{Toi,Toj}, {Toi,Toj-1}, {Ci,Cj}, Mindex};
    true -> void
  end,
  if ((Toi>=1) and (Toi=<N) and (Toj+1>=1) and (Toj+1=<N)) ->
    erlang:self() ! {{Toi,Toj}, {Toi,Toj+1}, {Ci,Cj}, Mindex};
    true -> void
  end,
  if ((Toi-1>=1) and (Toi-1=<N) and (Toj>=1) and (Toj=<N)) ->
    erlang:self() ! {{Toi,Toj}, {Toi-1,Toj}, {Ci,Cj}, Mindex};
    true -> void
  end.

% passNeighborsParallel - passing neighbors a new message targeted to to the Center - sending to other pids
passNeighborsParallel({{Fromi,Fromj}, {Toi,Toj}, {Ci,Cj},Mindex}) ->
  % define the 4 pids around
  PidRight = tileToPid(Toi,Toj+1), PidLeft = tileToPid(Toi,Toj-1),
  PidUp = tileToPid(Toi-1,Toj), PidDown = tileToPid(Toi+1,Toj),
  % send them a message { From, Receiver, Target, Num of message}
  try PidDown ! {{Fromi,Fromj}, {Toi+1,Toj}, {Ci,Cj}, Mindex} of
    _ -> ok
  catch
    _ -> void; _:_ -> void
  end,
  try PidLeft ! {{Fromi,Fromj}, {Toi,Toj-1}, {Ci,Cj}, Mindex} of
    _ -> ok
  catch
    _ -> void; _:_ -> void
  end,
  try PidRight ! {{Fromi,Fromj}, {Toi,Toj+1}, {Ci,Cj}, Mindex} of
    _ -> ok
  catch
    _ -> void; _:_ -> void
  end,
  try PidUp ! {{Fromi,Fromj}, {Toi-1,Toj}, {Ci,Cj}, Mindex} of
    _ -> ok
  catch
    _ -> void; _:_ -> void
  end.

% passdNeighborsSerial - passing neighbors a new messege targeted to to the Center - sending to SELF
passdNeighborsSerial(N,{{Fromi,Fromj}, {Toi,Toj}, {Ci,Cj},Mindex}) ->
  % send them a messege { From, Receiver, Target, Num of messege}
  if ((Toi+1>=1) and (Toi+1=<N) and (Toj>=1) and (Toj=<N)) ->
    erlang:self() ! {{Fromi,Fromj}, {Toi+1,Toj}, {Ci,Cj}, Mindex};
    true -> void
  end,
  if ((Toi>=1) and (Toi=<N) and (Toj-1>=1) and (Toj-1=<N)) ->
    erlang:self() ! {{Fromi,Fromj}, {Toi,Toj-1}, {Ci,Cj}, Mindex};
    true -> void
  end,
  if ((Toi>=1) and (Toi=<N) and (Toj+1>=1) and (Toj+1=<N)) ->
    erlang:self() ! {{Fromi,Fromj}, {Toi,Toj+1}, {Ci,Cj}, Mindex};
    true -> void
  end,
  if ((Toi-1>=1) and (Toi-1=<N) and (Toj>=1) and (Toj=<N)) ->
    erlang:self() ! {{Fromi,Fromj}, {Toi-1,Toj}, {Ci,Cj}, Mindex};
    true -> void
  end.

% monitor_receive - will be the process of the monitor. will stop the system at the end.
%                   will save all msgs that C got
monitor_receive(MsgBoxCounter,N,M,Root,{Ci,Cj},Parallel) ->
  % monitor will get only {Mindex} messages
  receive
    {Mindex} ->                                                                 % monitor got a messege indexed Mindex
      %io:format("monitor received: updating #~p counter to ~p.~n",[Mindex,1+maps:get(Mindex,MsgBoxCounter)]),    % TODO: Comment
      MsgBoxCounter2 = maps:update_with(Mindex,fun(V) -> V+1 end,MsgBoxCounter), % update the message counter + 1
      case got_all_received(MsgBoxCounter2,N*N-1,M) of
        true ->                                                   % we got all the messeges - nobody is sending more messages
          %io:format("monitor received: C got all the messeges back.~n"),   % TODO: Comment
          case Parallel of
            true ->
              terminate_all(1,1,N,{Ci,Cj}),                      % terminate all process and back to the main function
              tileToPid(Ci,Cj) ! {finish};
            false ->
              worker ! {finish}                                  % killing the only worker
          end,
          Root ! {finish};
        false ->                                                          % not done yet
          monitor_receive(MsgBoxCounter2,N,M,Root,{Ci,Cj},Parallel)       % not finished - continue loop
      end
  end.

% receiveMesh - the prcs function
%               MainDict - is the messege filter database, won't look at a message that received twice
%               {Ci,Cj}  - the center tile indices
%               Parallel - mode of operation, to other pids or to self...
receiveMesh(MainDict, N, M, {Ci,Cj},T1, Parallel) ->
  receive
  % all messeges will be - {Sender, Receiver, Target Mindex}
    {start} ->                                                  % FOR C - C will send his neighbors M messages
      %io:format("{~p,~p} is the master, sent all messeges.~n",[Ci,Cj]),   % TODO: Comment
      case Parallel of
        true -> sendMeshParallel(N,M, {Ci,Cj});                   % send to other pids
        false -> sendMeshSerial(N,M, {Ci,Cj})                     % send to self process (but to other tiles)
      end,
      receiveMesh(MainDict, N, M, {Ci,Cj},T1, Parallel);
    {die,{_,_}} ->                                                % FOR ALL - process will stop working gracefully
      %io:format("{~p,~p} killed.~n",[Pi,Pj]),                    % TODO: Comment
      void;
    {finish} ->                                                   % FOR C - print time and num of messages
      io:fwrite("C: {~p,~p} received ~p messeges.~n",[Ci,Cj,M*(N*N-1)]),            % print num of messeges
      io:fwrite("C: Time for action: ~p miliseconds.~n",[round(timer:now_diff(os:timestamp(), T1) / math:pow(10,3))]); % print time

  % {Toi,Toj} - is the tile that got the messege
    {{Fromi,Fromj}, {Toi,Toj}, {Targeti,Targetj},Mindex} ->
      if
        {Targeti,Targetj}=:={to,all} ->
          Msg = lists:flatten(io_lib:format("~p",[[{to,all},Mindex]]));                 % saving the msg {to,all} - only 1 time sending a new message
        true ->
          Msg = lists:flatten(io_lib:format("~p",[[{Fromi,Fromj},{Toi,Toj},{Targeti,Targetj},Mindex]]))      % Msg to be saved
      end,
      TempMap = dict:fetch({Toi,Toj}, MainDict),                     % returning the Receiver message Map
      if
        is_list(TempMap) -> ReceiverMsgMap = lists:nth(1,TempMap);
        true -> ReceiverMsgMap = TempMap
      end,
      % checking if already got the message
      case maps:get(Msg, ReceiverMsgMap, firstTimeArrived) of
        % a. first time got the messege
        firstTimeArrived ->
          % add the messege to this receiver's MsgMap
          ReceiverMsgMap2 = maps:put(Msg,0,ReceiverMsgMap),
          % updating the Dictionary
          MainDict2 = dict:store({Toi,Toj}, ReceiverMsgMap2, MainDict),
          case {Toi,Toj}=:={Ci,Cj} of

            % b. Receiver is C
            true ->
              case {Targeti,Targetj}=:={Ci,Cj} of
                % c1. Target is also C - send monitor to count the messege
                true ->
                  monitor ! {Mindex};                       % sending monitor to count the Mindex - shouldn't have error
                false -> void
              end;

            % b. Receiver is other
            false ->
              case {Targeti,Targetj}=:={to,all} of
                % c2. Target is the receiver - {to,all} is general for all the N^2-1 tiles
                true ->
                  %io:format("{~p,~p} CREATED #~p messege and SENT it away to TARGET {~p,~p}.~n",[Toi,Toj,Mindex,Ci,Cj]),   % TODO: Comment
                % sending neighbors a new messege targeted to C
                  case Parallel of
                    % d21. parallel function - send neighbors pid
                    true ->
                      sendNeighborsParallel({{Fromi,Fromj}, {Toi,Toj}, {Ci,Cj},Mindex}),
                      passNeighborsParallel({{Fromi,Fromj}, {Toi,Toj}, {Targeti,Targetj},Mindex});
                    % d21. serial function - send neighbors self
                    false ->
                      sendNeighborsSerial(N,{{Fromi,Fromj}, {Toi,Toj}, {Ci,Cj},Mindex}),
                      passdNeighborsSerial(N,{{Fromi,Fromj}, {Toi,Toj}, {Targeti,Targetj},Mindex})
                  end;
                % c2. Target isn't the receiver - pass it
                false ->
                  %io:format("{~p,~p} PASSING #~p messege FROM {~p,~p}, and TARGET {~p,~p}.~n",[Toi,Toj,Mindex,Fromi,Fromj,Targeti,Targetj]),   % TODO: Comment
                  % passing neighbors the messege - changing {Toi,Toj} to mention the receiver
                  case Parallel of
                    % d21. parallel function - send neighbors pid
                    true ->
                      passNeighborsParallel({{Fromi,Fromj}, {Toi,Toj}, {Targeti,Targetj},Mindex});
                    % d21. serial function - send neighbors self
                    false ->
                      passdNeighborsSerial(N,{{Fromi,Fromj}, {Toi,Toj}, {Targeti,Targetj},Mindex})
                  end
              end
          end,
          receiveMesh(MainDict2, N, M, {Ci,Cj},T1, Parallel);
        % a. not the first time - ignore the message
        _ ->
          receiveMesh(MainDict, N, M, {Ci,Cj},T1, Parallel)
      end
  end.

%% mesh_serial - the same like before but with 1 process
mesh_serial(N,_,C) when C<1 orelse C>N*N -> io:format("Bad arguments~n");         % error
mesh_serial(N,_,_) when N<1 -> io:format("Bad arguments~n");                      % error
mesh_serial(_,M,_) when M<1 -> io:format("Bad arguments~n");                      % error
mesh_serial(N,M,C) ->
  T1 = os:timestamp(),                                                              % make the initial timestamp
  Root = self(),
  MainDict_1 = dict:new(),                                                          % creating dictionary
  MainDict = create_maindict (N,1,1, MainDict_1),                                   % -"-
  {Ci,Cj} = getRowCol(C,N),                                                         % returns the place of the Center
  MessageBoxCounter = create_msg_box(M,maps:new()),                                 % map to record the msg index counter
  register (monitor, spawn(fun() -> monitor_receive(MessageBoxCounter,N,M,Root,{Ci,Cj},false) end)),   % creating the monitor process
  register (worker, spawn(fun() -> receiveMesh(MainDict, N, M, {Ci,Cj},T1, false) end)),         % process is only 1 worker
  worker ! {start},                                                       % sending the Center a start message
  receive
    Ans ->
      Ans                                                                           % wait till the end
  end,
  Time = round(timer:now_diff(os:timestamp(), T1) / math:pow(10,3)),
  {Time,M,M*(N*N-1)}.                                                               % if we got here, this will always be the same.
