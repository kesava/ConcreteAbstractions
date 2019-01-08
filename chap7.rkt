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