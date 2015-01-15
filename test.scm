(define (f x y) (+ x y))
(write (f 1 2))

(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
(write (factorial 10))

(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
(define my-count (counter 5))

(write (my-count 3))
(write (my-count 6))
(write (my-count 5)) 

