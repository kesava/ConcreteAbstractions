(#%require racket/trace)

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
    (begin
      ;(display (car (car pattern)))
      ;(display "\n")
    (cond
      ((eq? (car (car pattern)) (car pmse))
       (= (length (cdr (car pattern))) (length (cdr pmse))))
      ((and (equal? (car pattern) '(...)) (list? pmse))
       #t)
      (else #f)))))

(define substitutions-in-to-match
  (lambda (pattern pmse)
    (define subs-internal
      (lambda (pattern pmse acc)
        (cond
          ((null? pattern) acc)
          ((eq? (car pattern) '_)
           (subs-internal (cdr pattern) (cdr pmse) (cons (car pmse) acc))))))
    (if (null? (cdr pattern))
        (cons pmse '())
        (reverse (subs-internal (cdr pattern) (cdr pmse) '())))))

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
      
;;; parse and evaluate
(define parse
  (lambda (expression)
    (define loop
      (lambda (p/a-list)
        (begin
         ;(display (car p/a-list))
         ;(display "\n")
        (cond
          ((null? p/a-list) (display "No valid expression"))
          ((matches? (car p/a-list) expression)
           (apply (action (car p/a-list)) (substitutions-in-to-match (pattern (car p/a-list)) expression)))
          (else (loop (cdr p/a-list))))))) ;; end of loop
    (begin
      ;(display expression)
      ;(display "\n")
      (cond
       ((name? expression) (make-name-ast expression))
       ((or (number? expression)
            (string? expression)
            (boolean? expression))
        (make-constant-ast expression))
       ((list? expression)
        (loop micro-scheme-parsing-p/a-list))
       (else (display "not a valid expression"))))))

(define micro-scheme-parsing-p/a-list
  (list
   (make-pattern/action '(if _ _ _)
                       (lambda (test if-true if-false)
                         (make-conditional-ast (parse test)
                                               (parse if-true)
                                               (parse if-false))))
   (make-pattern/action '(lambda _ _)
                        (lambda (parameters body)
                          (if (and (list? parameters)
                                   ((all-are name?) parameters))
                              (make-abstraction-ast parameters (parse body))
                              (error "invalid expression" (list 'lambda parameters body)))))
   (make-pattern/action '(quote _)
                        (lambda (value)
                          (make-constant-ast value)))
   (make-pattern/action '(let _ _)
                        (lambda (bindings body)
                          (define split-binding-as-ast (lambda (binding) (make-binding-ast (make-var-name-ast (car binding)) (parse (car (cdr binding))))))
                          (make-let-expression-ast (map split-binding-as-ast bindings) (parse body))))
   (make-pattern/action '(...)
                        (lambda (operator&operands)
                          (let ((asts (map parse operator&operands)))
                            (begin
                              ;(print asts)
                              (make-application-ast (car asts) (cdr asts))))))))

(define evaluate
  (lambda (ast)
    (ast 'evaluate)))

(define evaluate-in
  (lambda (ast env)
    ((ast 'evaluate-in) env)))

(define substitute-for-in
  (lambda (value name ast)
    ((ast 'substitute-for) value name)))

(define print
  (lambda (args)
    (if (list? args)
        (cond
          ((null? args) (display "\n"))
          (else
           (begin
             (if (procedure? (car args))
                 (print (car args))
                 (display (car args)))
             (print (cdr args)))))
    (args 'print))))

(define pow
  (lambda (base power)
    (define pow-gen
      (lambda (base power acc)
        (if (= power 1)
            acc
            (pow-gen base (- power 1) (* acc base)))))
    (pow-gen base power base)))

(define incr  (lambda (x) (+ x 1)))
(define ln (lambda (x) (/ (log x) (log 10))))

(define make-pair
  (lambda (var value)
     (cons var (cons value '()))))

(define update-pair
  (lambda (list var value)
    (cond
      ((null? list) (cons (make-pair var value) '()))
      ((eq? (car (car list)) var) (cons (make-pair var value) (cdr list)))
      (else (cons (car list) (update-pair (cdr list) var value ))))))

(define get-value
  (lambda (list var)
    (cond
      ((null? list) '())
      ((eq? (car (car list)) var) (car (cdr (car list))))
      (else (get-value (cdr list) var)))))

;; Format of table
;; (list of variable-value pairs)
;; '((x 0) (y 3) (z 9))
(define variable-lookup
  (lambda (lookuptable)
    (lambda (message)
      (cond
        ((eq? message 'set)
         (lambda (name value)
           (variable-lookup (update-pair lookuptable name value))))
        ((eq? message 'get)
         (lambda (name)
           (get-value lookuptable name)))
        ((eq? message 'list) lookuptable)
        (else (display "unknown operation on lookup table"))))))

(define make-env
  (lambda ()
  (variable-lookup '())))

(define reduce-env
  (lambda (env list)
    (cond
      ((null? list)
       env)
      (else (reduce-env ((env 'set) (car (car list)) (car (cdr (car list)))) (cdr list))))))

(define look-up-value
  (lambda (name env)
    (env 'get) name))

(define look-up-procedure
       (lambda (name . env)
         (cond ((equal? name '+) +)
               ((equal? name '*) *)
               ((equal? name '-) -)
               ((equal? name '/) /)
               ((equal? name '^) pow)
               ((equal? name '++) incr)
               ((equal? name 'ln) ln)
               ((equal? name '<) <)
               ((equal? name '>) >)
               ((equal? name '==) =)
               ;(else (begin
                       ;(display "Unsupported procedure")
               ;        #f)))))
               ;(else name))))
               (else (((variable-lookup env) 'get) name)))))

(define supported-procedure? look-up-procedure)

(define make-var-name-ast
 (lambda (varname)
   (lambda (message)
     (cond
       ((equal? message 'print)
        (begin
          (print (cons "<Variable-AST -> " (cons varname '())))))
       ((equal? message 'evaluate)
        varname)
       ((equal? message 'evaluate-in)
        (lambda (env)
          ((env 'get) varname)))))))

(define make-name-ast
  (lambda (name)
    (define the-ast
      (lambda (message)
        (cond           
          ((equal? message 'evaluate) (look-up-procedure name))
          ((equal? message 'evaluate-in)
           (lambda (env)
             (look-up-procedure name env)))
          ((equal? message 'print)
           (begin
             (print (cons "<Name-AST -> " (cons name '())))))
          ((equal? message 'substitute-for)
           (lambda (value name-to-substitute-for)
             (if (equal? name name-to-substitute-for)
                 (make-constant-ast value)
                 the-ast)))
          (else (display "unknown operation on a name ASt")))))
    the-ast))

(define make-constant-ast
  (lambda (value)
    (define the-ast
      (lambda (message)
        (cond
          ((equal? message 'print)
           (begin
             (print (cons "<Constant AST -> " (cons value '())))))
          ((equal? message 'evaluate) value)
          ((equal? message 'evaluate-in)
           (lambda (env)
             the-ast))
          ((equal? message 'substitute-for)
           (lambda (value name)
             the-ast))
          (else (display "Unknown operation on constant AST")))))
    the-ast))

(define make-conditional-ast
  (lambda (test-ast if-true-ast if-false-ast)
    (lambda (message)
      (cond
        ((equal? message 'print)
         (print (cons "<Conditional AST -> " (cons test-ast (cons if-true-ast (cons if-false-ast '()))))))
        ((equal? message 'evaluate)
         (if (evaluate test-ast)
             (evaluate if-true-ast)
             (evaluate if-false-ast)))
        ((equal? message 'substitute-for)
         (make-conditional-ast
          (substitution-for-in value name test-ast)
          (substitution-for-in value name if-true-ast)
          (substitution-for-in value name if-false-ast)))
        (else (display "unknown operation on conditional AST"))))))                
      
(define make-application-ast
  (lambda (operator-ast operand-asts)
    (lambda (message)
      (cond
        ((equal? message 'print)
         (print (cons "<Application AST -> operator-ast: " (cons operator-ast (cons "; operand-asts: " (cons operand-asts (cons "\n" '())))))))
        ((equal? message 'evaluate)
         (let ((procedure (evaluate operator-ast))
               (arguments (map evaluate operand-asts)))
           (begin
             (display "\neval: ")
             (print procedure)
             ;(print arguments)
           (apply procedure arguments))))
        ((equal? message 'evaluate-in)
          (lambda (env)
            (begin
              (print (cons operator-ast '()))
            (cond
              ((procedure? (evaluate operator-ast))
                (let ((procedure (evaluate-in operator-ast env))
                      (arguments (map evaluate operand-asts)))
                  (apply procedure arguments)))
              (else
               
               ((env 'set) (evaluate operator-ast) (evaluate (car operand-asts))))))))
        ((equal? message 'substitute-for)
         (lambda (value name)
           (make-application-ast
            (substitute-for-in value name operator-ast)
            (map (lambda (operand-ast) (substitute-for-in value name operand-ast)) operand-asts))))
        (else (display "unknown message for opplication AST"))))))

(define make-abstraction-ast
 (lambda (parameters body-ast)
   (define the-ast
     (lambda (message)
       (cond
         ((equal? message 'print)
          (print (cons "<Abstraction AST -> parameters: " (cons parameters (cons " body-ast: " (cons body-ast '()))))))
         ((equal? message 'evaluate)
          (make-procedure parameters body-ast))
         ((equal? messsage 'evaluate-in)
          (lambda (env)
            (evaluate the-ast)))
         ((equal? message 'substitute-for)
          (lambda (value name)
            (if (member name parameters)
                the-ast
                (make-abstraction-ast
                 parameters
                 (substitute-for-in value name body-ast)))))
         (else (display "Illegal operation on abstraction AST")))))
   the-ast))

(define make-procedure
  (lambda (parameters body-ast)
    (begin
      ;(display "parameters: ")
      ;(display parameters)
    (lambda arguments
      (define loop
        (lambda (parameters arguments body-ast)
          (begin
            (display "\n parameers ")
            (display parameters)
            (print body-ast)
          (cond
            ((null? parameters)
             (if (null? parameters)
                 ;body-ast
                 (evaluate body-ast)
                 (display "too many args")))
            ((null? arguments)
             (display "too few arguments"))
            (else
             (loop (cdr parameters) (cdr arguments) (substitute-for-in (car arguments) (car parameters) body-ast)))))))
      (loop parameters arguments body-ast)))))

(define make-binding-ast
  (lambda (var-name-ast body-ast)
    ;(let ((var-name-ast (car (car pair))) (body-ast (car (cdr (car pair)))))
    (lambda (message)
      (cond
        ((eq? message 'print)
         (print (cons "< Binding AST -> " (cons var-name-ast (cons body-ast '())))))
        ((eq? message 'varname)
         (evaluate var-name-ast))
        ((eq? message 'evaluate-in)
          (lambda (env)
           (begin
             (print (env 'list))
             (print var-name-ast)
             (print body-ast)
           ((env 'set) (evaluate var-name-ast) (evaluate body-ast)))))
        ((eq? message 'substitute-for)
         (lambda (value name-to-substitute-for)
                  (begin
           (display "\n<< Substitute Binding AST : ")
           (display value)
           (print var-name-ast)
           (display  name-to-substitute-for)
           (print body-ast)
           (if (eq? (evaluate var-name-ast) name-to-substitute-for)
               (make-binding-ast var-name-ast body-ast)
               (make-binding-ast var-name-ast (substitute-for-in value name-to-substitute-for body-ast))))))
        (else (print "Unknown Operation on Binding AST"))))))


(define make-let-expression-ast
  (lambda (bindings-asts body-ast)
    (lambda (message)
      (cond
        ((eq? message 'print)
         (print (cons "< LET expression AST -> " (cons bindings-asts (cons "Body : " (cons body-ast '()))))))
        ((eq? message 'evaluate)
         (evaluate-in (make-let-expression-ast bindings-asts body-ast) (make-env)))
        ((eq? message 'evaluate-in)
         (lambda (env)
           (define loop
             (lambda (bindings env)
               (if (null? bindings)
                   env
                   (begin
                     (print (env 'list))
                     (loop (cdr bindings) (evaluate-in (car bindings) env))))))
             (let ((local-env (loop bindings-asts env)))
               (let (( local-vars (map car (local-env 'list))) (local-vals (map cadr (local-env 'list))))
                 (begin
                   (print (local-env 'list))
                   (print local-vars)
                   (print local-vals)
                   (display (make-procedure local-vars body-ast))
                   (apply (make-procedure local-vars body-ast) local-vals))))))
        ((eq? message 'substitute-for)
         (begin
          (print body-ast)
          (lambda (value name)
            (let ((local-vars (map (lambda (binding) (binding 'varname)) bindings-asts)))
              (begin
                (display "\n<<--")
                (print local-vars)
                (display value)
                (display name)
                (print body-ast)
                (display "-->>\n")
              (if (member name local-vars)
                  (make-let-expression-ast bindings-asts body-ast)
                  (make-let-expression-ast
                   (map (lambda (binding-ast) (substitute-for-in value name binding-ast)) bindings-asts)
                   (substitute-for-in value name body-ast))))))))
        (else (display "Unknown operation on LET AST"))))))

(trace evaluate-in)
         
           
                           
