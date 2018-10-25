
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define (sequence low high stride) (if (<= low high) (cons low (sequence (+ low stride) high stride)) null))

(define (string-append-map xs suffix) (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (let ([p (s)])
    (if (= n 0)
        null
        (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))))

(define funny-number-stream (letrec ([helper (lambda (n)
                                               (cons (if (= 0 (remainder n 5)) (- n) n) (lambda () (helper (+ n 1)))))])
                              (lambda () (helper 1))))

(define dan-then-dog (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

(define (stream-add-zero s) (let ([p (s)])
                               (lambda () (cons (cons 0 (car p)) (stream-add-zero (cdr p))))))

(define (cycle-lists xs ys) (letrec ([helper (lambda (n) (cons (cons
                                                                (list-nth-mod xs n)
                                                                (list-nth-mod ys n))
                                                               (lambda () (helper (+ n 1)))))])
                              (lambda () (helper 0))))

(define (vector-assoc v vec) (letrec ([len (vector-length vec)]
                                      [helper (lambda (i)
                                                (if (< i len)
                                                    (if (pair? (vector-ref vec i))
                                                        (if (equal? (car (vector-ref vec i)) v)
                                                            (vector-ref vec i)
                                                            (helper (+ i 1)))
                                                        (helper (+ i 1)))
                                                    #f))])
                               (helper 0)))

(define (cached-assoc xs n) (letrec ([cache (make-vector n #f)]
                                     [i 0])
                              (lambda (v) (let ([val (vector-assoc v cache)])
                                            (if val (cdr val) (let ([val (assoc v xs)]
                                                              [next-i (if (= i (- (vector-length cache) 1)) 0 (+ i 1))])
                                                          (if val
                                                              (begin
                                                                (vector-set! cache i (cons v val))
                                                                (set! i next-i)
                                                                val)
                                                              val)))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2) (letrec ([condition e1]
                                 [helper (lambda () (if
                                                     (> condition e2)
                                                     (helper)
                                                     #t))])
                             (helper))]))