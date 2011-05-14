-module(mobius).
 
-export([is_prime/1]).
-export([prime_factors/1]).
 
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
        factor2(N, [], 2).
 
factor2(1, [], _) -> [1];
factor2(N, Result, N) -> Result ++ [N];
factor2(N, Result, M) when N rem M =/= 0
                -> factor2(N, Result, M+1);
factor2(N, Result, M) when N rem M =:= 0
                -> factor2(N div M, Result ++ 
