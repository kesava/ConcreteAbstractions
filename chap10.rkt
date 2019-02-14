(define keyword?
  (lambda (arg)
    (cond
      ((eq? arg 'lambda)
       #t)
      ((eq? arg 'quote)
       #t)
      ((eq? arg 'if)
       #t)
      (else #f))))

(define name?
  (lambda (arg)
    (cond
      ((keyword? arg)
       #f)
      ((symbol? arg)
       #t)
      (else #f))))

;; <digit> ==> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
;; <unsinged-integer> ==> (<digit>(<digit>*))
;; <sign> ==> - | +
;; <integer> ==> ((<sign>?)<unsigned-integer>)
;; <decimal-point> ==> .
;; <real-number> ==> ((<sign>?) <unsigned-integer> (<decimal-point>?) <unsigned-integer>)

;; <binding> ==> (<symbol> <value>)
;; <letexp> => (let (<binding>+) <exp>)

;; <cond-exp> ==> (cond (<cond-exp>+) (else <cond-exp>))

(define matches?
  (lambda (pattern pmse)
    (cond
      ((eq? (car pattern) (car pmse))
       (= (length (cdr pattern)) (length (cdr pmse))))
      (else #f))))

(define substitutions-in-to-match
  (lambda (pattern pmse)
    (define subs-internal
      (lambda (pattern pmse acc)
        (cond
          ((null? pattern) acc)
          ((eq? (car pattern) '_)
           (subs-internal (cdr pattern) (cdr pmse) (cons (car pmse) acc))))))
    (reverse (subs-internal (cdr pattern) (cdr pmse) '()))))

(define syntax-ok?
  (lambda (pmse)
    (define loop
      (lambda (p/a-list)
        (cond
          ((null? p/a-list) #f)
          ((matches? (pattern (car p/a-list)) pmse)
           (apply (action (car p/a-list))
                  (substitutions-in-to-match
                   (pattern (car p/a-list))
                   pmse)))
          (else (loop (cdr p/a-list)))))) ;; end of loop
    (cond
      ((or (number? pmse)
           (string? pmse)
           (boolean? pmse)
           (null? pmse))
       #t)
      ((name? pmse) #t)
      ((list? pmse)
       (loop micro-scheme-syntax-ok?-p/a-list))
      (else #f))))

(define make-pattern/action
  (lambda (pattern action)
    (cons pattern action)))

(define pattern car)
(define action cdr)

(define all-are
  (lambda (oper)
    (define all-are-internal
      (lambda (collection)
        (cond
          ((null? collection) #t)
          ((oper (car collection))
           (all-are-internal (cdr collection)))
          (else #f))))
    all-are-internal))

(define micro-scheme-syntax-ok?-p/a-list
  (list
   (make-pattern/action '(if _ _ _)
                        (lambda (test if-true if-false)
                          (and (syntax-ok? test)
                               (syntax-ok? if-true)
                               (syntax-ok? if-false))))
   (make-pattern/action '(lambda _ _)
                        (lambda (params body)
                          (and (list? params)
                               ((all-are name?) params)
                               (syntax-ok? body))))
   (make-pattern/action '(quote _)
                        (lambda (datum) #t))
   (make-pattern/action '(...)
                        (lambda (pmses)
                          ((all-are syntax-ok?) pmses)))))


(define repl-syntax
  (lambda ()
    (display "; Enter a Micro-Scheme expression: ")
    (newline)
    (let ((expression (read)))
        (display "The syntax is ")
        (write (syntax-ok? expression))
        (newline))
    (repl-syntax)))
          