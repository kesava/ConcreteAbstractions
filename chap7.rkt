; cdr down a list
(define integers-from-to
  (lambda (from to)
    (if (> from to)
        '()
        (cons from (integers-from-to (+ 1 from) to)))))
        
(define sum
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum (cdr lst))))))

(define mymap
  (lambda (fn acc lst)
    (if (null? lst)
        acc
        (fn (car lst) (mymap fn acc (cdr lst))))))

(define how-many
  (lambda (item lst)
    (if (null? lst)
        0
        (if (= (car lst) item)
            (+ 1 (how-many item (cdr lst)))
            (how-many item (cdr lst))))))

(define how-many-predicate
  (lambda (item predicate lst)
    (if (null? lst)
        0
        (if (predicate (car lst) item)
            (+ 1 (how-many-predicate item predicate (cdr lst)))
            (how-many-predicate item predicate (cdr lst))))))

(define my-list-ref
  (lambda (lst n)
    (if (null? lst)
        #f
        (if (= n 0)
            (car lst)
            (my-list-ref (cdr lst) (- n 1))))))

(define any?
  (lambda (predicate lst)
    (if (null? lst)
        #f
        (if (predicate (car lst))
            #t
            (any? predicate (cdr lst))))))

(define position
  (lambda (item lst)
    (position-iter item lst 0)))

(define position-iter
  (lambda (item lst pos)
    (if (null? lst)
        #f
        (if (= (car lst) item)
            pos
            (position-iter item (cdr lst) (+ 1 pos))))))


(define list-<
  (lambda (lst1 lst2)
    (cond
      ((and (null? lst1) (null? lst2))
       #t)
      ((< (car lst1) (car lst2))
       (list-< (cdr lst1) (cdr lst2)))
      (else
       #f))))

(define filter
  (lambda (ok? lst)
    (cond
      ((null? lst)
       '())
      ((ok? (car lst))
       (cons (car lst) (filter ok? (cdr lst))))
      (else
       (filter ok? (cdr lst))))))

(define first-elements-of
  (lambda (n list)
    (if (= n 0)
        '()
        (cons (car list) (first-elements-of (- n 1) (cdr list))))))

(define leng-iter
  (lambda (list len)
    (if (null? list)
        len
        (leng (cdr list) (+ 1 len)))))

(define my-list-tail
  (lambda (list n)
    (if (= n 0)
        list
        (list-tail (cdr list) (- n 1)))))

(define interleave
  (lambda (lst1 lst2)
    (if (null? lst1)
        lst2
        (cons (car lst1) (interleave lst2 (cdr lst1))))))

(define shuffle
  (lambda (deck size)
    (let ((half (quotient (+ size 1) 2)))
      (interleave (first-elements-of half deck) (my-list-tail deck half)))))
               
(define multiple-shuffle
  (lambda (deck size times)
    (if (= times 0)
        deck
        (multiple-shuffle (shuffle deck size) size (- times 1)))))

(define in-order?
  (lambda (lst)
    (cond
      ((null? lst)
       #t)
      ((null? (cdr lst))
       #t)
      ((< (car lst) (cadr lst))
       (in-order? (cdr lst)))
      (else
       #f))))

(define shuffle-number-iter
  (lambda (deck size iter)
    (if (in-order? deck)
        iter
        (let ((next-iter (+ 1 iter)))
          (shuffle-number-iter (multiple-shuffle deck size next-iter) size next-iter)))))

(define shuffle-number
  (lambda (num)
    (let ((deck (integers-from-to 1 num)))
      (shuffle-number-iter (shuffle deck num) num 1))))

(define sevens
  (lambda (rep)
    (map (lambda (x) (values 7)) (integers-from-to 1 rep))))

(define list-of-lists
  (lambda (lst)
    (map (lambda (x) (integers-from-to 1 x)) lst)))

(define my-map
  (lambda (fn lst)
    (if (null? lst)
        '()
        (cons (fn (car lst)) (my-map fn (cdr lst))))))

(define add-to-end
  (lambda (lst elt)
    (if (null? lst)
        (cons elt '())
        (cons (car lst) (add-to-end (cdr lst) elt)))))

(define reverse-onto
  (lambda (lst1 lst2)
    (if (null? lst1)
        lst2
        (reverse-onto (cdr lst1) (cons (car lst1) lst2)))))

(define my-reverse
  (lambda (lst)
    (reverse-onto lst '())))
   
(define merge-sort
  (lambda (lst)
    (cond
      ((null? lst)
       '())
      ((null? (cdr lst))
       lst)
      (else
       (merge (merge-sort (one-part lst))
              (merge-sort (the-other-part lst)))))))

(define merge
  (lambda (lst1 lst2)
    (cond
      ((null? lst1)
       lst2)
      ((null? lst2)
       lst1)
      ((< (car lst1) (car lst2))
       (cons (car lst1) (merge (cdr lst1) lst2)))
      ((= (car lst1) (car lst2))
       (cons (car lst1) (merge (cdr lst1) (cdr lst2))))
      (else
       (cons (car lst2) (merge lst1 (cdr lst2)))))))

(define odd-part
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (car lst) (even-part (cdr lst))))))

(define even-part
  (lambda (lst)
    (if (null? lst)
        '()
        (odd-part (cdr lst)))))

(define one-part odd-part)
(define the-other-part even-part)
       
(define square
  (lambda (x)
    (* x x)))

(define cube
  (lambda (x)
    (* x x x)))

(define function-sum
  (lambda (fns)
    (if (null? fns)
        '()
        (lambda (n) (apply + (map (lambda (f) (f n)) fns))))))

(define apply-all
  (lambda (fns num)
    (map (lambda (f) (f num)) fns)))

(define map2
  (lambda (fn lst1 lst2)
    (if (and (null? lst1) (null? lst2))
        '()
        (cons (fn (car lst1) (car lst2)) (map2 fn (cdr lst1) (cdr lst2))))))

(define repeat
  (lambda (num times)
    (if (= times 0)
        '()
        (cons num (repeat num (- times 1))))))

(define rle-expand
  (lambda (lst)
    (cond
      ((null? lst)
       '())
      ((null? (cdr lst))
       lst)
      ((if (number? (car lst))
           (append (repeat (cadr lst) (car lst)) (expand (cdr (cdr lst))))
           (cons (car lst) (expand (cdr lst))))))))

(define make-list-combiner
  (lambda (fn)
    (lambda (lst1 lst2)
      (if (and (null? lst1) (null? lst2))
          '()
          (cons (fn (car lst1) (car lst2)) (map2 fn (cdr lst1) (cdr lst2)))))))


;; A Movie Query System
(define make-movie
  (lambda (title director year-made actors)
    (list title director year-made actors)))
(define movie-title car)
(define movie-director cadr)
(define movie-year-made caddr)
(define movie-actors cadddr)


(define our-movie-database
  (list (make-movie '(amarcord)
                    '(federico fellini)
                    1974
                    '((magali noel) (bruno zanin)
                                    (pupella maggio)
                                    (armando drancia)))
        (make-movie '(the big easy)
                    '(jim mcbride)
                    1987
                    '((dennis quaid) (ellen barkin)
                                     (ned beatty)
                                     (lisa jane persky)
                                     (john goodman)
                                     (charles ludlam)))
        (make-movie '(the godfather)
                    '(francis ford coppola)
                    1972
                    '((marlon brando) (al pacino)
                                      (james caan)
                                      (robert duvall)
                                      (diane keaton)))
        (make-movie '(boyz n the hood)
                    '(john singleton)
                    1991
                    '((cuba gooding jr.) (ice cube)
                                         (larry fishburne)
                                         (tyra ferrell)
                                         (morris chestnut)))))


(define make-movies-made-in
  (lambda (db key)
    (lambda (value)
      (map movie-title (filter (lambda (movie) (equal? (key movie) value)) db)))))

(define movies-satisfying
  (lambda (db filterFn keyFn)
    (map keyFn (filter filterFn db))))

(define query-loop
  (lambda ()
    (newline)
    (newline)
    (let ((query (read)))
      (cond ((exit? query) (display '(see you later)))
            ;; movie-p/a-list is the list of the
            ;; pattern/action pairs
            (else (answer-by-pattern query movie-p/a-list)
                  (query-loop))))))
(define exit?
  (lambda (query)
    (member query '((bye)
                    (quit)
                    (exit)
                    (so long)
                    (farewell)))))

(define answer-by-pattern
  (lambda (query p/a-list)
    (cond
      ((null? p/a-list)
       (display '(i do not understand)))
      ((matches? (pattern (car p/a-list)) query)
       (let ((subs (substitution-in-to-match (pattern (car p/a-list)) query)))
         (let ((result (apply (action (car p/a-list)) subs)))
           (if (null? result)
               (display '(i do not know))
               (display result)))))
      (else
       (answer-by-pattern query (cdr p/a-list))))))
             

(define make-pattern/action
  (lambda (pattern action)
    (cons pattern action)))

(define pattern car)
(define action cdr)

(define movie-p/a-list
  (list (make-pattern/action
         '(who is the director of ...)
         (lambda (title)
           (movies-satisfying
            our-movie-database
            (lambda (movie) (equal? (movie-title movie) title))
            movie-director)))
        (make-pattern/action
         '(who acted in ...)
         (lambda (title)
           (movies-satisfying
            our-movie-database
            (lambda (movie) (equal? (movie-title movie) title))
            movie-actors)))
        (make-pattern/action
         '(what (movies movie) (was were) made (before in after) ...)
         (lambda (matches)
           (let ((range (caddr matches)) (year (cadddr matches)))
           (cond
             ((equal? range 'in)
              (movies-satisfying
               our-movie-database
               (lambda (movie) (= (movie-year-made movie) (car year)))
               movie-title))
             ((equal? range 'before)
              (movies-satisfying
               our-movie-database
               (lambda (movie) (< (movie-year-made movie) (car year)))
               movie-title))
             ((equal? range 'after)
              (movies-satisfying
               our-movie-database
               (lambda (movie) (> (movie-year-made movie) (car year)))
               movie-title))
             ))))))

(define matches?
  (lambda (pattern question)
    (cond
      ((and (null? pattern) (null? question))
       #f)
      ((null? question)
       #f)
      ((equal? (car pattern) '...)
       #t)
      ((list? (car pattern))
       (if (null? (member (car question) (car pattern)))
           #f
            (matches? (cdr pattern) (cdr question))))
      ((not (list? (car pattern)))
       (if (equal? (car pattern) (car question))
           (matches? (cdr pattern) (cdr question))
           #f))
      (else
       #f))))


(define substitution-in-to-match-internal
  (lambda (pattern question accumulator)
    (cond
      ((null? pattern)
       accumulator)
      ((equal? (car pattern) '...)
       (cons question accumulator))
      ((list? (car pattern))
       (if (null? (member (car question) (car pattern)))
           #f
           (substitution-in-to-match-internal (cdr pattern) (cdr question) (cons (car question) accumulator))))
      ((not (list? (car pattern)))
       (if (equal? (car pattern) (car question))
           (substitution-in-to-match-internal (cdr pattern) (cdr question) accumulator)
           #f))
      (else
       '()))))

(define substitution-in-to-match
  (lambda (pattern question)
    (reverse (substitution-in-to-match-internal pattern question '()))))
   
