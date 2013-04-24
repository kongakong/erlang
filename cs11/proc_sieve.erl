-module(proc_sieve).

-export([generate/1]).
-export([sieve2/2]).

-define(TRACE(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-define(TIMEOUT, 1000000).

%%
%% Use processes/0 to check if there is any process leak
%% 

sieve2(0, InvalidPid) ->
    receive 
        P -> sieve2(P, InvalidPid)
    after ?TIMEOUT ->
        ?TRACE("time out in P=0~n")
    end; 

%% starting condition
sieve2(P, NextPid) when is_pid(NextPid) ->
    receive 
        {done, From} ->
            NextPid ! {done, self()},
            receive 
                LstOfRes -> 
                    From ! [P] ++ LstOfRes 
            end;
        N when N rem P == 0 -> 
            sieve2(P, NextPid); %% this semicolon is needed
        N when N rem P /= 0 -> 
            NextPid ! N,
            sieve2(P, NextPid) %% put semicolon here causes syntax error
    after ?TIMEOUT ->
        ?TRACE(io:format("time out in is_pid clause P=~p~n", [P]))
    end;
 sieve2(P, Invalid) ->
    receive 
        {done, From} ->
            %% no downstream process, just send the result back 
            From ! [P];
        N when N rem P == 0 -> 
            sieve2(P, Invalid); %% this semicolon is needed
        N when N rem P /= 0 -> 
            ?TRACE(io:format("Starting ~p for ~p~n", [self(), N])), 
            Pid = spawn(proc_sieve, sieve2, [0, void]),
            Pid ! N,
            sieve2(P, Pid) %% put semicolon here causes syntax error
    after ?TIMEOUT ->
        ?TRACE(io:format("time out in no pid clause P=~p~n", [P]))
    end.
    
sieve() ->
    spawn(proc_sieve, sieve2, [0, void]).

generate(MaxN) ->
        Pid = sieve(),
        generate2(Pid, 2, MaxN).

generate2(Pid, End, End) ->
        Pid ! {done, self()},
        receive
                Res -> Res
        end;

generate2(Pid, N, End) ->
        Pid ! N,
        generate2(Pid, N + 1, End).

