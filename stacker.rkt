;; Stacker from Beautiful Racket by Matthew Butterick

;; 4
;; 8
;; 12
;; +
;; 23
;; -
;; done
;; result: -1

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
            (repl-internal (cons (apply (eval r (scheme-report-environment 5)) list) '())))
          (else (repl-internal (cons r list)))))))
  (repl-internal '()))
