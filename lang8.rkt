; lang8.rkt
#lang racket
(require syntax/strip-context)

(define (read in)
  (read-syntax #f in))
(provide read)

(define (read-syntax path port)
  (with-syntax ([str (filter non-empty-string? (port->lines port))])
    (strip-context
     #'(module anything racket
         (require racket)
         (define store (list))
         (define highest-val 0)
         
         (provide store)

         (define (not-equal? a b)
           (not (equal? a b)))
         
         (define (handle target-var dir delta cmp-var test cmp-val)
           (define (read-var var)
             (if (not (assoc var store))
                 0
                 (cdr (assoc var store))))
           (define (update-var! var val)
             (when (> val highest-val) (set! highest-val val))
             (set! store (cons (cons target-var new-val)
                               (filter (lambda (binding) (not (equal? (car binding) var)))store))))
           (define old-val (read-var target-var))
           (define new-val (if (equal? dir "inc")
                               (+ old-val (string->number delta))
                               (- old-val (string->number delta))))
           (when (equal? test "==")
             (set! test "equal?"))
           (when (equal? test "!=")
             (set! test "not-equal?"))
           (when ((eval (string->symbol test)) (read-var cmp-var) (string->number cmp-val))
             (update-var! target-var new-val)))

         (define (run)
           (set! store (list))
           (for ([line 'str])
             ;(printf "line = ~a~n" line)
             (define args (filter (lambda (word) (not (equal? "if" word))) (string-split line)))
             (eval (cons handle args)))
           (for ([binding (sort store (lambda (a b) (< (cdr a) (cdr b))))])
             (printf "~a = ~a~n" (car binding) (cdr binding)))
           (printf "highest value ever seen: ~a~n" highest-val))))))
(provide read-syntax)

