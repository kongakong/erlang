-module(proc_sieve).

-export([generate/1]).
-export([sieve2/1]).

-define(TRACE(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).


is_multiple_of(_, []) ->
    false;
is_multiple_of(N, [H|T]) when N rem H /= 0 ->
    is_multiple_of(N, T);
is_multiple_of(N, [H|_]) when N rem H == 0 ->
    true.

sieve2([]) ->
    receive
        N -> void
    end,
    sieve2([N]); 
        
sieve2(Res) ->
    receive 
        {done, From} -> 
            From ! Res;
        N -> 
            case is_multiple_of(N, Res) of
            true ->
                sieve2(Res); %% this semicolon is needed
            false ->
                sieve2(Res ++ [N]) %% put semicolon here causes syntax error
            end %% end if 
    end.
    
sieve() ->
    spawn(proc_sieve, sieve2, [[]]).

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

