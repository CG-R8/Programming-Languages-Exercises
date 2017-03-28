;;-*- mode: scheme; -*-
;; :set filetype=scheme

;;Return a 2-element list containing the 2 roots of the quadratic
;;equation a*x^2 + b*x + c = 0 using the classical formula for the
;;roots of a quadratic equation.  The first element in the returned list
;;should use the positive square-root of the discriminant, the second
;;element should use the negative square-root of the discriminant.
(define (quadratic-roots a b c)
  '())

;;Return the list resulting by multiplying each element of `list` by `x`.
(define (mul-list list x)
 '())


;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.
(define (sum-lengths list)
  '())

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.
(define (poly-eval coeffs x)
  '())

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.
(define (poly-eval-horner coeffs x)
  '())

;;Return count of occurrences equal? to x in exp
(define (count-occurrences exp x)
  '())

;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.
(define (eval-arith exp)
  '())

;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.
(define (sum-lengths-tr list)
  '())

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.
(define (poly-eval-tr coeffs x)
  '())

;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.
(define (mul-list-2 list x)
  '())

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.
(define (sum-lengths-2 list)
  '())
