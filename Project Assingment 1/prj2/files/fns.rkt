#lang racket

;;-*- mode: scheme; -*-
;; :set filetype=scheme

;;Return a 2-element list containing the 2 roots of the quadratic
;;equation a*x^2 + b*x + c = 0 using the classical formula for the
;;roots of a quadratic equation.  The first element in the returned list
;;should use the positive square-root of the discriminant, the second
;;element should use the negative square-root of the discriminant.
(define (quadratic-roots a b c)
  (if (= a 0)
      (if (= b 0)
          ;;If both the coefficients are zero then it is not an equation
          (displayln "Not quadratic equation") (- (/ c b)))
      ;;Calculate the discriminant of quadratic equation
      (let ((discriminant  (- (* b b) (* 4 a c))))
        (if (and (real? discriminant ) (> discriminant  0))
            ;;calculate the numerator part of roots
            (let ((top (+ b (* (if (>= b 0) 1 -1) (sqrt discriminant )))))
              (list (/ top -2 a) (/ (* -2 c) top)))
            (list
             (/ (- (sqrt discriminant ) b) 2 a)
             (/ (+ (sqrt discriminant ) b) -2 a))))))

;;Return the list resulting by multiplying each element of `list` by `x`.
(define (mul-list list x)
  ;;check if list is empty
  (if (empty? list)
      empty
      ;;else multiply the first element of list to x
      (cons (* (car list) x)
            ;;recursive call with tail of list
            (mul-list (cdr list) x))))


;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.
(define (sum-lengths list)
  (if (null? list)
      0
      ;;Hold the value of length of fisrt sub-list then recursive call 
      (+ (length (car list)) (sum-lengths (rest list)))))

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.
(define (poly-eval coeffs x)
  (define len (length coeffs))
  (if (null? coeffs)
      0
      ;;calculate the first (i th) element of coeff and multiply with x^i
      ;;add the multiple expression using recursion
      (+ (*(car coeffs) (expt x (- len 1)))
         (poly-eval (cdr coeffs) x))
      )
  )

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.
(define (poly-eval-horner coeffs x)
  ;;Define new internal function which take temp as additional parameter as 0 
  (define (internal_fun coeffs x temp) 
    (if (null? coeffs)
        ;if null them tree the returning value 0
        temp
        ;;calculate the addition of adjecient terms
        (internal_fun (cdr coeffs) x (+ (* temp x) (car coeffs)))
        )
    )
  (internal_fun coeffs x 0)
  )

;;Return count of occurrences equal? to x in exp
(define (count-occurrences s-exp x)
  ;;if both lists are equal then return 1 to every recursive call
  (if(equal? s-exp x)
     1
     (if   (pair? s-exp)
           ;;Do the list deep traverse 
           (+(count-occurrences (car s-exp) x)
             (count-occurrences (cdr s-exp) x)
             )
           ;;if the list is not equal and not a pair ,return 0
           0) 
     )
  )

;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.
(define (eval-arith expr)
  (cond
    ;;Check if expression is number or not
    ((number? expr)
     expr)
    (else
     (let ((op (car expr))    ;;Store the operator type in op
           (exp_1 (eval-arith (cadr expr))) ;;Take next element of op as 1st number
           (exp_2 (eval-arith (caddr expr)))) ;;Take next element as 2st number
       ;;Perform the corresponding operations of "op" over exp1 and exp2
       (cond ((eq? op 'add)
              (+ exp_1 exp_2))
             ((eq? op 'sub)
              (- exp_1 exp_2))
             ((eq? op 'mul)
              (* exp_1 exp_2))
             ((eq? op 'div)
              (/ exp_1 exp_2))
             (else
              ;;If the input operator is wrong 
              (displayln "Input is wrong"))))
     )))


;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.
(define (sum-lengths-tr list)
  ;;Create the auxilary function with acc as accumulator
  (letrec ([aux-sum-lengths-tr
            (lambda (acc list)   ;;for all elements of list
              (if (null? list)
                  acc           ;;if list is numm, retun value of accumulator
                  ;;else recusive call, which is at the end of function
                  (aux-sum-lengths-tr (+ acc (length (car list))) (cdr list))
                  )
              )])
    ;;initicall values
    (aux-sum-lengths-tr 0 list)
    )
  )

;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.
(define (poly-eval-tr coeffs x)
  ;;Create the auxilary function with acc as accumulator
  (letrec ([aux-poly-eval-tr
            (lambda (acc coeffs x)
              (define len (length coeffs))  ;;Store the lenght of coefficients as len
              (if (null? coeffs)
                  acc
                  ;;Evaluate each sub expression od polynomial with recursive call at its tail
                  (aux-poly-eval-tr (+ acc (* (car coeffs) (expt x (- len 1)))) (cdr coeffs) x)
                  )
              )])
    (aux-poly-eval-tr 0 coeffs x)
    )
  )

;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.
(define (mul-list-2 list x)
  (map (lambda (element)   ;;map each itme of list with "element" and perform multiplication 
         (* x element)) list)
  )

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.
(define (sum-lengths-2 list)
  (foldr (lambda (element result) ;;;;accmulates the results of addition over the list from right side to left
           (+ result (length element)))
         0 list)
  )
