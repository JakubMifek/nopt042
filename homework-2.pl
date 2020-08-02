% Fibonacci numbers
fib(N, N, X, _, X).
fib(N, M, A, B, X):-N > M, M2 is M+1, C is A+B, fib(N, M2, B, C, X).

fibonacci(0, 1).
fibonacci(N, X):-N > 0, fib(N, 0, 0, 1, X).
