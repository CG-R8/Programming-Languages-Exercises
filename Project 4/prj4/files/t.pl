eval_arith(plus(X, Y), Z):-
  eval_arith(X, X1),
  eval_arith(Y, Y1),
  Z is X1 + Y1.
eval_arith(times(X, Y), Z):-
  eval_arith(X, X1),
  eval_arith(Y, Y1),
  Z is X1 * Y1.
eval_arith(X, X):-
  integer(X).

