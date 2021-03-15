(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (x) (append x first)) rests))

(define (zip pairs)
  (begin
    (define firsts
      (map (lambda (x) (car x)) pairs))
    (define lasts 
      (map (lambda (x) (car (cdr x))) pairs))
    (cons firsts (cons lasts nil))))

;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (enumerate-tail lst n result)
    (if (null? lst) result
      (enumerate-tail (cdr lst) (+ n 1) (append result (cons (cons n (cons (car lst) nil)) nil)))))
  (enumerate-tail s 0 '()))
  ; END PROBLEM 16

;; Problem 17
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 17
    (begin 
      (define result '())
      ((cond ((< total (car denoms))
        (list-change total (cdr denoms)))
              ((eq? total (car denoms))
                (cons-all (cons (cons (car denoms) nil) nil) result))
              (())
  

 ))))
  ; END PROBLEM 17

;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (cons form (cons params (map let-to-lambda body)))))
           ; END PROBLEM 18
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
            (begin 
              (define zipped (zip values))
              (define first (car zipped))
              (append (list (list 'lambda first (car (map let-to-lambda body)))) (map let-to-lambda (car (cdr zipped)))))))
           ; END PROBLEM 18
        (else
         ; BEGIN PROBLEM 18
         ; END PROBLEM 18
         (cons (car expr) (map let-to-lambda (cdr expr))))))
