(define sequence-from-to
  (lambda (low high)
    (lambda (op)
      (cond
        ((equal? op 'empty-sequence)
         (> low high))
        ((equal? op 'head)
         low)
        ((equal? op 'tail)
         (sequence-from-to (+ low 1) high))
        ((equal? op 'sequence-length)
         (if (> low high) 0 (+ (- high low) 1)))
        (else
         (error "illegeal sequence operation"))))))

(define sequence->list
  (lambda (seq)
     (if (seq 'empty-sequence)
       '()
       (cons (seq 'head) (sequence->list (seq 'tail))))))

(define sequence-with-from-by
  (lambda (len start by)
      (lambda (op)
        (cond
          ((equal? op 'empty-sequence)
           (<= len 0))
          ((equal? op 'head)
           start)
          ((equal? op 'tail)
           (sequence-with-from-by (- len 1) (+ start by) by))
          (else
           (error "illegal seq"))))))
(define sequence-from-to-2
  (lambda (low high)
    (sequence-with-from-by (+ (- high low) 1) low 1)))
         