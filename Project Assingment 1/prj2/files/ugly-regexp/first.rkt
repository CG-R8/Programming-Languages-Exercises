#lang racket


(define (sum-lengths lst)  
  (if (null? lst)
      "" ; <===
      (string-append (number->string (car (car lst))) 
                     ", " 
                     (number->string (length (cdr (car lst)))) 
                     "\n" ; <===
                     (sum-lengths (cdr lst)))))

(define (mul-list list x)
  (if (empty? list)
      empty
      (cons (* (car list) x)
            (mul-list (cdr list) x))))

(define (pp coeffs x)
  (define len (length coeffs))
  (if(null? coeffs)
     0
     (+ (* (car coeffs) (expt x (- len 1))        ) (pp (cdr coeffs) x))))


(define (qq a b c)
	(if (= a 0)
	(if (= b 0) (displayln "Not quadratic equation") (- (/ c b)))
	(let ((delta (- (* b b) (* 4 a c))))
	(if (and (real? delta) (> delta 0))
		(let ((u (+ b (* (if (>= b 0) 1 -1) (sqrt delta)))))
			(list (/ u -2 a) (/ (* -2 c) u)))
		(list
			(/ (- (sqrt delta) b) 2 a)
			(/ (+ (sqrt delta) b) -2 a))))))



