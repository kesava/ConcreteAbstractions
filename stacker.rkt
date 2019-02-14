;; Stacker from Beautiful Racket by Matthew Butterick

(define (read-eval-print)
  (display "read-eval-print here")
  (newline)
  (display "type done to exit")
  (newline)
  (define repl-internal
    (lambda (list)
      (let ((r (read)))
        (cond
          ((eq? r 'done)
           (begin
             (display "Final result: ")
             (write list)
             (newline)
             (display "bye!")))
          ((or (eq? r '+) (eq? r '-) (eq? r '*) (eq? r '/))
           (begin
             (write (eval (cons r list) (scheme-report-environment 5)))
             (newline)
             (repl-internal (cons (eval (cons r list) (scheme-report-environment 5)) '()))))
          (else (repl-internal (cons r list)))))))
  (repl-internal '()))
