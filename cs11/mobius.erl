-module(mobius).

-export([is_prime/1]).
-export([prime_factors/1]).
-export([is_square_multiple/1]).
-export([find_square_multiples/2]).

is_prime(N) ->
is_prime_test(N, trunc(math:sqrt(N))).


is_prime_test(1, _) -> true;
is_prime_test(2, _) -> true;
is_prime_test(_, 1) -> true; %% termination condition
is_prime_test(Testee, N) when Testee rem N =:= 0
-> false;
is_prime_test(Testee, N) when Testee rem N =/= 0
-> is_prime_test(Testee, N-1).


prime_factors(N) ->
    prime_factors2(N, [], 2).

prime_factors2(1, [], _) -> [1];
prime_factors2(N, Result, N) -> Result ++ [N];
prime_factors2(N, Result, M) when N rem M =/= 0
    -> prime_factors2(N, Result, M+1);
prime_factors2(N, Result, M) when N rem M =:= 0
    -> prime_factors2(N div M, Result ++ [M] , 2).


is_square_multiple(N) ->
    Res = prime_factor_freq([], prime_factors(N)),
    Res2 = lists:filter(fun({_, F}) -> F > 1 end, Res),
    length(Res2) >= 1.


prime_factor_freq(Res, [H|T]) ->
    FullLen = length([H|T]),
    Tmp = lists:filter(fun(X) -> X =/= H end, T),
    prime_factor_freq([{H, FullLen - length(Tmp)}] ++ Res, Tmp);
prime_factor_freq(Res, []) ->
    Res.

%% By definition the Count must be bigger than 1
is_square_multiple2(N, Count) when Count > 1 ->
    Res = prime_factor_freq([], prime_factors(N)),
    Res2 = lists:filter(fun({_, F}) -> F >= Count end, Res),
    length(Res2) >= 1.

%% Part 5
%% Examples
%% 20> mobius:find_square_multiples(3, 50).
%% 48
%% 21> mobius:find_square_multiples(3, 20).
%% fail
find_square_multiples(Count, MaxN) ->
    find_square_multiples2(Count, MaxN, []).


%%
%% from 2 to MaxN
%%
%% Walking backwards
%%
find_square_multiples2(Count, ToTest, Found) when erlang:length(Found) == Count  -> 
    %% io:format("~w~n", [Found]),
    ToTest+1;
find_square_multiples2(_, 2, _) ->
    fail; %% search failed
find_square_multiples2(Count, ToTest, Found) ->
    case is_square_multiple(ToTest) of 
        true -> NewFound = Found ++ [ToTest];
        _ -> NewFound = [] % reset
    end,
    % io:format("~w~n", [NewFound]),
    find_square_multiples2(Count, ToTest-1, NewFound).

