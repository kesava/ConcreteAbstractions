(#%require (only racket/base random))

(define make-empty-tree
  (lambda () '()))

(define make-nonempty-tree
  (lambda (root left-subtree right-subtree)
    (list root left-subtree right-subtree)))

(define empty-tree? null?)

(define root car)
(define left-subtree cadr)
(define right-subtree caddr)

(define in?
  (lambda (value tree)
    (cond
      ((empty-tree? tree) #f)
      (= value (root tree) #t)
      (< value (root tree) (in? value (left-subtree tree)))
      (else ; Must be in the right subtree
       (in? value (right-subtree tree))))))

(define minimum
  (lambda (tree)
    (if (empty-tree? tree)
        #f
        (if (empty-tree? (left-subtree tree))
            (root tree)
            (minimum (left-subtree tree))))))

(define num-of-nodes
  (lambda (tree)
    (if (empty-tree? tree)
        0
        (+ 1 (num-of-nodes (left-subtree tree)) (num-of-nodes (right-subtree tree))))))

(define preorder
  (lambda (tree)
    (if (empty-tree? tree)
        '()
        (cons (root tree) (append (preorder (left-subtree tree)) (preorder (right-subtree tree)))))))

(define inorder
  (lambda (tree)
    (if (empty-tree? tree)
        '()
        (append (inorder (left-subtree tree)) (cons (root tree) (inorder (right-subtree tree)))))))

(define postorder
  (lambda (tree)
    (if (empty-tree? tree)
        '()
        (append (postorder (left-subtree tree)) (postorder (right-subtree tree)) (list (root tree))))))

(define insert
  (lambda (value tree)
    (cond
      ((empty-tree? tree) (make-nonempty-tree value '() '()))
      ((= (root tree) value) tree)
      ((> (root tree) value) (make-nonempty-tree (root tree) (insert value (left-subtree tree)) (right-subtree tree)))
      (else (make-nonempty-tree (root tree) (left-subtree tree) (insert value (right-subtree tree)))))))

(define list->bstree-internal
  (lambda (lst tree)
    (cond
      ((null? lst) tree)
      (else (list->bstree-internal (cdr lst) (insert (car lst) tree))))))

(define list->bstree
  (lambda (lst)
    (list->bstree-internal lst (make-empty-tree))))

(define binary-sort
  (lambda (lst)
    (inorder (list->bstree lst))))

(define first-elements-of
  (lambda (n list)
    (if (or (= n 0) (null? list))
        '()
        (cons (car list) (first-elements-of (- n 1) (cdr list))))))

(define my-length
  (lambda (lst)
    (define leng-iter
      (lambda (list len)
        (if (null? list)
            len
            (leng-iter (cdr list) (+ 1 len)))))
    (leng-iter lst 0)))

(define my-list-tail
  (lambda (list n)
    (if (or (= n 0) (null? list))
        list
        (my-list-tail (cdr list) (- n 1)))))

(define half
  (lambda (lst)
    (quotient (my-length lst) 2)))


(define my-list-ref
  (lambda (lst n)
    (if (null? lst)
        #f
        (if (= n 0)
            (car lst)
            (my-list-ref (cdr lst) (- n 1))))))
        
(define median
  (lambda (lst)
    (my-list-ref lst (half lst))))

(define integers-from-to
  (lambda (from to)
    (if (> from to)
        '()
        (cons from (integers-from-to (+ 1 from) to)))))

(define three-parts-median
  (lambda (sorted-lst)
    (list (first-elements-of (half sorted-lst) sorted-lst) (median sorted-lst) (my-list-tail sorted-lst (+ (half sorted-lst) 1)))))

(define make-optimized-bstree-internal
  (lambda (lst tree)
    (cond
      ((null? lst) tree)
      (else (list->bstree-internal (cdr lst) (insert (car lst) tree))))))

(define list->optimized-bintree
  (lambda (lst three-parts-fn)
    (let ((threeparts (three-parts-fn lst)))
      (let ((root (cadr threeparts)) (left-lst (car threeparts)) (right-lst (caddr threeparts)))
      (cond
        ((and (null? left-lst) (null? right-lst))
         (make-nonempty-tree root '() '()))
        ((null? left-lst)
         (make-nonempty-tree root '() (list->optimized-bintree right-lst three-parts-fn)))
        ((null? right-lst)
         (make-nonempty-tree root (list->optimized-bintree left-lst three-parts-fn) '()))
        (else
         (make-nonempty-tree root (list->optimized-bintree left-lst three-parts-fn) (list->optimized-bintree right-lst three-parts-fn))))))))

(define list->optimized-bstree
  (lambda (lst)
    (list->optimized-bintree (binary-sort lst) three-parts-median)))

(define at-position
  (lambda (lst value)
    (define at-position-iter
      (lambda (lst pos)
        (if (= (car lst) value)
           pos
           (at-position-iter (cdr lst) (+ 1 pos)))))
    (if (null? (member value lst))
          #f
          (at-position-iter lst 0))))

(define move-max-to-the-car
  (lambda (lst)
    (let ((pos (at-position lst (apply max lst))))
      (append (list (apply max lst)) (first-elements-of pos lst) (my-list-tail lst (+ pos 1))))))
               
(define three-parts-max-car-position
  (lambda (lst)
    (let ((maxlst (move-max-to-the-car lst)) (len (length (cdr lst))))
      (list (first-elements-of (+ (quotient len 2) 1) (cdr maxlst)) (car maxlst) (my-list-tail (cdr maxlst) (+ (quotient len 2) 1))))))

(define list->priorityqueue
  (lambda (lst)
    (list->optimized-bintree lst three-parts-max-car-position)))

(define count-nodes
  (lambda (tree)
    (if (or (null? tree) (null? (root tree)))
        0
        (+ 1 (count-nodes (left-subtree tree)) (count-nodes (right-subtree tree))))))

(define depth-tree
  (lambda (tree)
    (cond
      ((or (null? tree) (null? (root tree)))
       0)
      ((and (null? (left-subtree tree)) (null? (right-subtree tree)))
           1)
      ((null? (left-subtree tree))
       (+ 1 (depth-tree (right-subtree tree))))
      ((null? (right-subtree tree))
       (+ 1 (depth-tree (left-subtree tree))))
      (else
       (+ 1 (max (depth-tree (left-subtree tree)) (depth-tree (right-subtree tree))))))))

(define generate-random
  (lambda (range howmany)
    (if (= howmany 0)
        '()
        (cons (random range) (generate-random range (- howmany 1))))))

;;;;;;;;;;;; trie

(define make-empty-trie
  (lambda () '()))

(define make-nonempty-trie
  (lambda (root-values ordered-subtries)
    (list root-values ordered-subtries)))

(define empty-trie? null?)
(define root-values car)
(define subtries cadr)

(define subtrie-with-label
  (lambda (trie label)
    (list-ref (subtries trie) (- label 2))))

(define make-person
  (lambda (name phone-number)
    (list name phone-number)))

(define name car)
(define phone-number cadr)

(define phone-trie '(() ((() ((() (() (() ((() (() () () () () () (() (() () () () () (() (() () () () () () () (((cadbury 7464)) (() () () () () () () ())))) () ())) ())) () () () () () () ())) () (() (() (() (() () () () () (((baker 7465)) (() () () () () () () ())) () ())) () (() (() (() ((() ((() (() () () () () () (() (() () () () () () (((callebaut 7480)) (() () () () () () () ())) ())) ())) () () () () () () ())) () () () () () () ())) () () () () () ())) () () () ())) () () () ())) () () () () () () ())) () (() (() (() (() () () () () (() (() () () () () (() (() () (() (() (() (() () () () () () () (((hershey 7482)) (() () () () () () () ())))) () () () () () ())) () () () () ())) () ())) () ())) (() (() () (() (() () () () () (() ((() (() (() (() (() (() () () (() (() () () (() (() () (((ghiradelli 7476)) (() () () () () () () ())) () () () () ())) () () () ())) () () () ())) () () () () () ())) () () () () () ())) () () () () () () ())) () ())) () () () () ())) () () () () ())) (() (() () (() (() () () () (() (() (() (() () () () () () (((lindt 7483)) (() () () () () () () ())) ())) () () () () () ())) () () ())) () () () () ())) (() ((() (() () (() (() () () (() (() () () (() ((() (() () () () () (() (() (((maillard 7477)) (() () () () () () () ())) () () () () () ())) () ())) () () () () () () ())) () () () ())) () () () ())) () () () () ())) (() (() () () () () (() (() () () (() (() (() (() () () () (() (() () () () () (((merkens 7469)) (() () () () () () () ())) () ())) () () ())) () () () () () ())) () () () ())) () ())) () () () () () ())) (() (() (() (() (((see 7463)) (() () () () () () () ())) () () () (() (() () () () () () (() (() () (() (() () (() (() () () () (() ((((perugina 7007)) (() () () () () () () ())) () () () () () () ())) () () ())) () () () () ())) () () () () ())) ())) () ())) (() (() () () () () () (() (() () () () () () (() (() (() (() () () () () (((ritter 7479)) (() () () () () () () ())) () ())) () () () () () ())) ())) ())) () () (() (() () () () () (() (() () () () () () (() (() (() (() () () () (() (() () (() (() () () (() (() () (((spruengli 7009)) (() () () () () () () ())) () () () () ())) () () () ())) () () () () ())) () () ())) () () () () () ())) ())) () ())) (() ((() (() () (() ((() (() () () () () (() (() (((suchard 7654)) (() () () () () () () ())) () () () () () ())) () ())) () () () () () () ())) () () () () ())) () () () () () () ())) ())) (() (() () () () (() ((() (() () () (() (() (() (() () () () () (((tobler 7481)) (() () () () () () () ())) () ())) () () () () () ())) () () () ())) () () () () () () ())) () () ())) (() (() () (() (() () () (() ((() (() () () () () () (() (() () () () () (((wilbur 7466)) (() () () () () () () ())) () ())) ())) () () () () () () ())) () () () ())) () () () () ())))))

(define all?
  (lambda (pred list)
    (cond
      ((null? list)
       #t)
      ((equal? (pred (car list)) #t)
       (all? pred (cdr list)))
      (else
       #f))))

(define identity
  (lambda (x)
    x))

(define number-in-trie
  (lambda (trie)
    (cond
      ((null? trie)
       0)
      ((and (null? trie) (null? (subtries trie)))
       0)
      ((null? (subtries trie))
       (length (root-values trie)))
      (else
       (+ (length (root-values trie)) (apply + (map number-in-trie (subtries trie))))))))


(define values-in-trie
  (lambda (trie)
    (cond
      ((null? trie)
       '())
      ((and (null? trie) (null? (subtries trie)))
       '())
      ((null? (subtries trie))
       (root-values trie))
      (else
       (append (root-values trie) (append (map values-in-trie (subtries trie))))))))

(define letter->number
  (lambda (letter)
    (cond
      ((or (eq? letter "A") (eq? letter "B") (eq? letter "C")
          2))
      (else
       7))))

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


(define successor-of-in-or-v2
  (lambda (value bst if-none)
    (cond ((empty-tree? bst)
           if-none)
          ((<= (root bst) value)
           (successor-of-in-or-v2 value (right-subtree bst) (cond
                                                           ((and (null? (right-subtree bst)) (null? (left-subtree bst))) ;; terminal case
                                                                if-none)
                                                            ((null? (right-subtree bst))
                                                                (if (= (root (left-subtree bst)) value)
                                                                    (root bst)
                                                                    if-none))
                                                            (else
                                                             (cond
                                                                ((= (root bst) value)
                                                                    (root (right-subtree bst)))
                                                                ((= (root (right-subtree bst)) value)
                                                                   (root (right-subtree bst)))
                                                                (else
                                                                 if-none))))))
          (else
           (successor-of-in-or-v2 value (left-subtree bst) (cond
                                                          ((and (null? (left-subtree bst)) (null? (right-subtree bst)))
                                                            if-none)
                                                          ((null? (left-subtree bst))
                                                              (if (= (root (right-subtree bst)) value)
                                                                    (root (right-subtree bst))
                                                                    if-none))
                                                          (else
                                                           (cond
                                                            ((> (root (left-subtree bst)) value)
                                                                (root (left-subtree bst)))
                                                            ((= (root (left-subtree bst)) value)
                                                               (root bst))
                                                            (else
                                                             if-none)))))))))
    


(define successor-of-in-or
  (lambda (value bst if-none)
    (cond
      ((empty-tree? bst)
       if-none)
      ((<= (root bst) value) ;; Look into the right subtree
       (successor-of-in-or value (right-subtree bst) (cond
                                              ((null? (right-subtree bst))
                                               if-none)
                                              ((< (root (right-subtree bst)) if-none) ;; if the right node is lesser, this is probably not your match.
                                                if-none)
                                              (else ;; Your match! 
                                                (root (right-subtree bst))))))
      (else
       ;; Look into the left subtree.
       ;; Having branched into the left subtree,
       ;; the original if-none value is no longer relevant,
       ;; as there is definitely a successor element in the tree.
       ;; The challenge is to retain the successor element thru if-none parameter.
       (successor-of-in-or value (left-subtree bst) (cond 
                                              ((< value (root bst)) ;; 
                                               (root bst))
                                              ((null? (left-subtree bst))
                                               if-none)
                                              ((< value (root (left-subtree bst)))
                                               ;; Retain the root node, as it might be a successor.
                                               (max if-none (root bst)))
                                              (else
                                               (max (root bst) (root (left-subtree bst))))))))))

;; (map (lambda (x) (successor-of-in-or x (list->optimized-bstree (integers-from-to 1 100)) 200)) (integers-from-to 1 100))
;; (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 200)
;; (in-order? (map (lambda (x) (successor-of-in-or x (list->optimized-bstree (integers-from-to 1 100)) 200)) (integers-from-to 1 100)))
;; #t


(define bst-range
  (lambda (bst lower-bound higher-bound)
    (define internal
      (lambda (bst lower higher output)
        (cond
          ((empty-tree? bst)
           '())
          ((< (root bst) lower-bound)
            (internal (right-subtree bst) lower higher output))
          ((> (root bst) higher-bound)
           (internal (left-subtree bst) lower higher output))
          (else
           (append (cons (root bst) '()) (internal (left-subtree bst) lower-bound (root bst) output) (internal (right-subtree bst) (root bst) higher-bound output))))))
    (internal bst lower-bound higher-bound '())))

(define bst-range-count
  (lambda (bst lower higher)
    (length (bst-range bst lower higher))))
