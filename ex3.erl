%%%-------------------------------------------------------------------
%%% @author omerlux
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 4 April 2019 12:19 PM
%%%-------------------------------------------------------------------
-module(ex3).
-author("omerlux").

%%API
-export([sortResLC/1, sortResPM/1, sortResLM/1, qSort/1, mSort/1, matElemMult/2, filter_g/2, filter_p/2, even/1, fiboR/1, fiboT/1]).

%%%-------------------------------------------------------------------
%% sortRes - sort list according to the residue of divison by 3
% sortResLC - List Comprehension ||
sortResLC(List) ->  mSort([X|| X <-List, X rem 3==0]) ++
                    mSort([X|| X <-List, X rem 3==1]) ++
                    mSort([X|| X <-List, X rem 3==2]).               % combining 3 lists together with mSort
%%%-------------------------------------------------------------------
% sortResPM - Pattern matching ()
sortResPM(List) -> sortResPM([],[],[],List).    % creating 3 lists of the rems

sortResPM(Rem0,Rem1,Rem2,[]) -> mSort(Rem0) ++ mSort(Rem1) ++ mSort(Rem2);          % merge them to 1 list
sortResPM(Rem0,Rem1,Rem2,[H|T]) when H rem 3==0 -> sortResPM([H|Rem0],Rem1,Rem2,T); % rem=0
sortResPM(Rem0,Rem1,Rem2,[H|T]) when H rem 3==1 -> sortResPM(Rem0,[H|Rem1],Rem2,T); % rem=1
sortResPM(Rem0,Rem1,Rem2,[H|T]) when H rem 3==2 -> sortResPM(Rem0,Rem1,[H|Rem2],T). % rem=2
%%%-------------------------------------------------------------------
% sortResLM - By using lists module, lists: and fun(X)
sortResLM(List) -> lists:sort( lists:filter(fun(X)-> X rem 3==0 end, List)) ++  % rem=0, using fun and lists modules
                   lists:sort( lists:filter(fun(X)-> X rem 3==1 end, List)) ++  % rem=1
                   lists:sort( lists:filter(fun(X)-> X rem 3==2 end, List)).    % rem=2
%%%-------------------------------------------------------------------
%% mSort - merge sort
mSort([]) -> [];
mSort([T])-> [T];
mSort(List) -> {List1,List2} = lists:split( length(List) div 2, List), % spliting each time to 2
                mergeIt ( mSort(List1), mSort(List2), [] ).            % merge all splits - using mergeIt

mergeIt([],L2,Res) -> Res++L2;                                                  % adding what's left
mergeIt(L1,[],Res) -> Res++L1;                                                  % adding what's left
mergeIt([H1|T1], [H2|T2], Res) when H1>=H2 -> mergeIt([H1|T1], T2, Res++[H2]);  % merging by H1,H2 order
mergeIt([H1|T1], [H2|T2], Res) when H1<H2  -> mergeIt(T1, [H2|T2], Res++[H1]).  % -"-
%%%-------------------------------------------------------------------
%% qSort - quick sort
qSort([]) -> [];
qSort([Pivot | List]) ->					% took a pivot
  qSort([X || X <- List, X < Pivot]) ++ 	% rearrange the list by the pivot, greater than the pivot or smaller than it
  [Pivot] ++
  qSort([X || X <- List, X >= Pivot]).
%%%-------------------------------------------------------------------
%% matElemMult(A,B) - multiply matrix A and B
matElemMult(A,B) -> lists:zipwith (   % A,B are matrix
  fun(As, Bs) -> lists:zipwith(       % As,Bs are Lists of rows - fun1 is for lists
    fun(X,Y) -> X*Y+0.0 end, As, Bs       % X,Y are elements that will be multiply - fun2 is for elements. 0.0 for float = no strings
  ) end, A, B).                       % zipwith is combining the lists by the fun()
%%%-------------------------------------------------------------------
%% filter_g(List,Filter) - filter the atom 'numbers' or 'atoms' and clear them - USING GUARDS
filter_g(List,Filter) when Filter==atoms -> ([X || X<-List, is_integer(X) orelse is_float(X)]); % is_integer/float=number
filter_g(List,Filter) when Filter==numbers -> ([X || X<-List, is_atom(X)]).                     % is_atom = atom
%%%-------------------------------------------------------------------
%% filter_p(List,Filter) - filter the atom 'numbers' or 'atoms' and clear them - USING FILTER
filter_p(List,atoms) -> ([X || X<-List, is_integer(X) orelse is_float(X)]); % is_integer/float=number
filter_p(List,numbers) -> ([X || X<-List, is_atom(X)]).                     % is_atom = atom
%%%-------------------------------------------------------------------
%% even(List) - returns a list of all even members of list in the same order that appeared (recursion)
even(List) -> even([],List).

even(Even,[]) -> Even;  % end of recursion
even(Even,[H|T]) when H rem 2 == 0 -> even(Even++[H],T); % using guards we will connect the next even integer
even(Even,[_|T]) -> even(Even,T).
%%%-------------------------------------------------------------------
%% fiboR(N) - retuns the n'th fibonacci number (recursion)
fiboR(1) -> 1;  % 1st fibonacci
fiboR(2) -> 1;  % 2nd fibonacci
fiboR(N) -> fiboR(N-1)+fiboR(N-2). %this is not a tail recursion
%%%-------------------------------------------------------------------
%% fiboT(N) - retuns the n'th finoacci number (tail recursion
fiboT(N) -> fiboT(N,0,1).   % fibonaci start

fiboT(1,Sum,_) -> Sum;      % finished recursion - inverse calculation - start to end
fiboT(N,0,1) -> fiboT(N-1,1,1);
fiboT(N,1,1) -> fiboT(N-1,2,1);
fiboT(N,Sum,Last) -> fiboT(N-1, Sum+Last, Sum).

%% we can see that the tail recursion is fater than the regular one












