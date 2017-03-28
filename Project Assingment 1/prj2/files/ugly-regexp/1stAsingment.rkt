#lang racket
(define (pp coeffs x)
  (define len (length coeffs))
  (if(null? coeffs)
     0
     (+ (* (car coeffs) (expt x (- len 1))        ) (pp (cdr coeffs) x))))

(let loop ((times 10))
  (if (= times 0)
      (display "stopped")
      (begin (display "still looping...")
             (loop (- times 1)))))

(define (ll x )
  (do ((x x (cdr x))
       (sum 0 (+ sum (car x))))
    ((null? x) sum)))

(define (sum a)
  (for-each (lambda (x) (set! sum (+ sum x))) (car a))
  )

(define (cc c x)
           (define (iterate lst)
      (displayln (first lst))
            (set! lst (rest lst))
             )


  (define term 0)
  (display "Polynomial Exp \n")
  (let loop ((len (length c)))
    (if
     (= len 0)        0
     (  begin 
         
         (set! term (* (last c) (expt x len)) )
         (display term)
         
         ;(set! len (- len 1))
         (display "  ")
         (1 len)
         (display "  ")
               (displayln (last c))
         (display "\n  ")
   
         (set! c (rest c))
         ;(iterate c)
         (loop (- len 1))
         
         )
     )
    ) 
  )

(define (aa c)
  (define len (length c))
  (map (lambda (x) (expt x (- (len (- len 1)) 1))  ) c)
  ;(set! len (- len 1))
  )

