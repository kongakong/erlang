-module(fib).
 
-export([fib_p/1, fib_g/1, tail_fib/1]).
 
-export([test/0, test_t/0]).
 
 
%% fib_p: non-tail recursion version
fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) -> fib_p(N-1) + fib_p(N-2).
 
 
%% guard
fib_g(N) when N == 0 -> 0 ;
fib_g(N) when N == 1 -> 1;
fib_g(N) -> fib_g(N-1) + fib_g(N-2).
 
 
%% tail version
tail_fib(N)
        -> fib_t(N, 0, []).
 
fib_t(1, _, _) -> 1; %% special condition 1
fib_t(2, _, _) -> 1; %% special condition 2
fib_t(EndCond, 0, [])
                -> fib_t(EndCond, 1, [1]); %% initial condition 1
fib_t(EndCond, 1, [1])
                -> fib_t(EndCond, 2, [1, 1]); %% initial condition 2
fib_t(EndCond, Counter, [X,Y|Tail]) when EndCond =/= Counter
                -> fib_t(EndCond, Counter+1, [X+Y, X, Y|Tail]);
fib_t(N, N, [Head| _])
                -> Head. %% end condition
 
%% test
test() ->
        io:format("fib_p ~p~n", [fib_p(10)]),
        io:format("fib_g ~p~n", [fib_g(10)]),
        io:format("tail_fib ~p~n", [tail_fib(10)]).
 
test_t() ->
        timer:tc(fib, fib_g, [10]).

